# Predict ---------------------------------------

build_fit_formula <- function(parsedmodel) {
  f <- build_linear_predictor(parsedmodel$terms)

  if (!is.null(parsedmodel$general$offset)) {
    f <- expr_addition(f, parsedmodel$general$offset)
  }

  if (parsedmodel$general$is_glm == 1) {
    link <- parsedmodel$general$link
    f <- apply_inverse_link(f, link)
  }
  f
}

build_linear_predictor <- function(terms) {
  parsed_f <- map(
    terms,
    ~ {
      if (.x$is_intercept == 0) {
        cols <- map(.x$fields, lm_constructor)
        cols <- reduce_multiplication(cols)
        expr((!!cols * !!.x$coef))
      } else {
        if (.x$coef == 0) {
          NULL
        } else {
          expr(!!.x$coef)
        }
      }
    }
  )
  parsed_f <- purrr::discard(parsed_f, is.null)

  if (length(parsed_f) == 0) {
    return(expr(0))
  }

  reduce_addition(parsed_f)
}

apply_inverse_link <- function(f, link) {
  switch(
    link,
    "identity" = f,
    "logit" = expr_logistic(f),
    "log" = expr(exp(!!f)),
    "inverse" = expr(1 / (!!f)),
    "1/mu^2" = expr(1 / sqrt(!!f)),
    # The probit inverse link is `pnorm()`, which no SQL backend has, so it is
    # written as the Bowling et al. logistic approximation to the normal CDF
    # instead. This is the one inverse link here that is not exact: it costs
    # about 1e-4 of probability, enough that `tidypredict_test()` reports a
    # probit model as failing at its default threshold. Documented in the glm
    # article, since nothing about the returned formula reveals it.
    "probit" = expr(1 / (1 + exp(-0.07056 * (!!f)^3 - 1.5976 * (!!f)))),
    "cloglog" = expr(1 - exp(-exp(!!f))),
    "sqrt" = expr((!!f)^2),
    cli::cli_abort("Link {.val {link}} is not supported.")
  )
}

# Parse model --------------------------------------

#' @export
parse_model.lm <- function(model) parse_model_lm(model)

parse_model_lm <- function(model) {
  acceptable_formula(model)

  coefs <- as.numeric(model$coefficients)
  labels <- names(model$coefficients)
  vars <- names(attr(model$terms, "dataClasses"))

  # A rank-deficient (aliased) fit leaves the coefficients it could not
  # identify as `NA`. `predict()` drops those terms and still returns fitted
  # values, so the fit is well defined; dropping them here means
  # `tidypredict_fit()` works for such a model too.
  # Decompose the coefficient labels using the model's own term structure
  # rather than by guessing at their spelling. `NULL` when the model does not
  # record enough to do so, in which case `build_terms()` falls back to
  # `parse_label_lm()`.
  fields <- lm_fields(model, labels)

  keep <- !is.na(coefs)
  coefs <- coefs[keep]
  labels <- labels[keep]
  fields <- fields[keep]

  qr <- qr_inverse_lm(model)

  pm <- list()
  pm$general$model <- class(model)[[1]]
  pm$general$version <- 2
  pm$general$type <- "regression"
  pm$general$residual <- model$df.residual

  # `summary.glm()` reports the residual variance as `dispersion`; only
  # `summary.lm()` has a `sigma`. Reading `sigma` for a glm gives `NULL`, which
  # silently drops out of the interval expression and yields no rows.
  sigma2 <- if (inherits(model, "glm")) {
    summary(model)$dispersion
  } else {
    summary(model)$sigma^2
  }
  if (length(sigma2) > 0) {
    pm$general$sigma2 <- sigma2
  }
  if (!is.null(model$family$family)) {
    pm$general$family <- model$family$family
  }
  if (!is.null(model$family$link)) {
    pm$general$link <- model$family$link
  }
  if (!is.null(model$call$offset)) {
    pm$general$offset <- model$call$offset
  }
  pm$general$is_glm <- 0
  if (class(model)[[1]] == "glm") {
    pm$general$is_glm <- 1
  }
  pm$terms <- build_terms(coefs, labels, vars, qr = qr, fields = fields)
  as_parsed_model(pm)
}

# Inverse of the R factor of the model's QR decomposition, which the prediction
# interval is built from. `tidypredict_fit()` never needs it.
#
# For a rank-deficient fit the full R factor is singular, but its leading
# `rank` rows and columns are not, and they are exactly the columns whose
# coefficients are not `NA`. Returning `NULL` rather than aborting keeps
# `parse_model()` working for models that only need fitted values; the abort
# happens in `te_interval_lm()`, where the QR is actually required.
qr_inverse_lm <- function(model) {
  if (is.null(model$qr)) {
    return(NULL)
  }
  r <- qr.R(model$qr)
  rank <- model$qr$rank %||% min(dim(r))
  rank <- min(rank, nrow(r), ncol(r))
  r <- r[seq_len(rank), seq_len(rank), drop = FALSE]
  tryCatch(solve(r), error = function(cnd) NULL)
}

# Build the `terms` entries of a parsed model from a set of coefficients.
#
# `vars` is the set of variables the model was fit on, used to recognise
# interactions and factor levels in a coefficient label. Pass `NULL` for models
# fit from a numeric matrix, where each coefficient names a column directly.
# `qr` adds the inverse QR decomposition needed for prediction intervals, and
# `drop_zero` discards zero coefficients, as the penalised models do.
#
# `fields` short-circuits the label parsing: a list, parallel to `labels`, of
# ready-made field decompositions. `lm_fields()` builds it from the model's
# term structure, which is exact, where `parse_label_lm()` can only guess from
# the spelling of the label.
build_terms <- function(
  values,
  labels,
  vars = NULL,
  qr = NULL,
  drop_zero = FALSE,
  fields = NULL
) {
  values <- as.numeric(values)
  if (is.null(fields)) {
    fields <- vector("list", length(values))
  }
  terms <- map(seq_along(values), function(i) {
    value <- values[[i]]
    label <- labels[[i]]
    if (drop_zero && value == 0) {
      return(NULL)
    }
    term <- list(
      label = label,
      coef = value,
      is_intercept = as.integer(label == "(Intercept)"),
      fields = if (!is.null(fields[[i]])) {
        fields[[i]]
      } else if (is.null(vars)) {
        list(list(type = "ordinary", col = label))
      } else {
        parse_label_lm(label, vars)
      }
    )
    if (!is.null(qr)) {
      term$qr <- parse_qr_lm(label, qr)
    }
    term
  })

  if (drop_zero) {
    return(purrr::discard(terms, is.null))
  }
  terms
}

# Decompose every coefficient label of an `lm`-like model into fields.
#
# `parse_label_lm()` has to work out from the spelling of a label alone which
# variables it came from, which is ambiguous in both directions: a factor level
# containing `:` looks like an interaction, and a label that happens to equal
# another variable's name looks like an ordinary column. Neither can be
# resolved from the string.
#
# The model itself is not ambiguous. The `assign` attribute says which term
# each coefficient belongs to, `term.labels` names the variables that term
# multiplies, and `xlevels` lists the levels each of those variables had. That
# is enough to say exactly what a label means.
#
# Returns `NULL` when the model records too little to do this, so that the
# caller falls back to `parse_label_lm()`.
lm_fields <- function(model, labels, call = rlang::caller_env()) {
  term_fields(
    labels,
    model$terms,
    xlevels = model$xlevels,
    contrasts = model$contrasts,
    frame = model$model,
    # `lm()` records `assign` directly. Most other models do not.
    assign = model$assign %||%
      attr(model$qr$qr, "assign") %||%
      attr(model$x, "assign"),
    call = call
  )
}

# The same decomposition, for a model that keeps its term structure somewhere
# other than the usual `lm` components. `labels` are the coefficient labels, in
# model matrix order, with or without a leading `"(Intercept)"`.
term_fields <- function(
  labels,
  terms,
  xlevels = NULL,
  contrasts = NULL,
  frame = NULL,
  assign = NULL,
  call = rlang::caller_env()
) {
  if (is.null(terms) || length(labels) == 0) {
    return(NULL)
  }
  intercept <- labels == "(Intercept)"
  cols <- labels[!intercept]

  fields <- term_column_fields(
    terms,
    cols,
    xlevels,
    contrasts,
    frame,
    assign,
    call = call
  )
  if (is.null(fields)) {
    return(NULL)
  }

  out <- vector("list", length(labels))
  out[intercept] <- list(list(list(type = "ordinary", col = "(Intercept)")))
  out[!intercept] <- fields
  out
}

term_column_fields <- function(
  terms,
  cols,
  xlevels,
  contrasts,
  frame,
  assign,
  call = rlang::caller_env()
) {
  exact <- term_assign(terms, cols, xlevels, contrasts, frame, assign)
  fields <- NULL
  if (!is.null(exact$assign)) {
    term_labels <- attr(terms, "term.labels")
    classes <- attr(terms, "dataClasses")
    fields <- map2(exact$cols, exact$assign, function(col, term) {
      vars <- strsplit(term_labels[[term]], ":", fixed = TRUE)[[1]]
      matches <- match_label_fields(col, vars, xlevels, classes)
      if (length(matches) == 1) {
        return(matches[[1]])
      }
      if (length(matches) > 1) {
        cli::cli_abort(
          c(
            x = "Unable to tell which factor levels the coefficient
            {.val {col}} refers to.",
            i = "A level containing {.val :} makes it match
            {length(matches)} combinations of {.val {vars}}."
          ),
          call = call
        )
      }
      # No decomposition fits, which happens for a contrast that does not name
      # its columns after the levels.
      NULL
    })
    if (!any(map_lgl(fields, is.null))) {
      return(fields)
    }
  }

  # Nothing recorded the levels each factor had, so fall back to working the
  # decomposition out from how many columns there are. That counting argument
  # only holds when `cols` is the whole model matrix, so it is skipped when the
  # model matrix is known to have a different number of columns.
  if (!isTRUE(exact$complete)) {
    return(fields)
  }
  infer_term_fields(terms, cols) %||% fields
}

# The `assign` vector of the model matrix, restricted to its non-intercept
# columns, paired with the names that model matrix gave those columns.
#
# The names matter because some models mangle the labels they carry (duplicate
# model matrix column names get made unique downstream), while the names the
# model matrix itself hands out are the ones the decomposition understands.
term_assign <- function(terms, cols, xlevels, contrasts, frame, assign) {
  if (!is.null(assign)) {
    a <- assign[assign != 0]
    if (length(a) == length(cols)) {
      return(list(assign = a, cols = cols, complete = TRUE))
    }
  }

  mm <- term_model_matrix(terms, xlevels, contrasts, frame)
  if (is.null(mm)) {
    # Nothing is known about the model matrix, not even how wide it is.
    return(list(assign = NULL, cols = NULL, complete = TRUE))
  }
  a <- attr(mm, "assign")
  keep <- a != 0
  a <- a[keep]
  nms <- colnames(mm)[keep]

  if (length(a) == length(cols)) {
    return(list(assign = a, cols = nms, complete = TRUE))
  }
  # A model that only keeps some of the columns, such as one that drops the
  # features shrinkage removed. Names are the only way back, so they have to be
  # unambiguous.
  if (anyDuplicated(nms) == 0) {
    idx <- match(cols, nms)
    if (!anyNA(idx)) {
      return(list(assign = a[idx], cols = nms[idx], complete = FALSE))
    }
  }
  list(assign = NULL, cols = NULL, complete = FALSE)
}

# Rebuild the model matrix, so that its `assign` attribute can be read off.
#
# The model frame is used when the model kept one. Otherwise a stand-in frame
# is built from the levels the model recorded, which gives a model matrix with
# the same columns without needing the fitting data to still exist.
term_model_matrix <- function(terms, xlevels, contrasts, frame) {
  mm <- NULL
  if (!is.null(frame)) {
    mm <- tryCatch(
      stats::model.matrix(terms, frame, contrasts.arg = contrasts),
      error = function(cnd) NULL
    )
  }
  if (!is.null(mm)) {
    return(mm)
  }

  frame <- synthetic_frame(terms, xlevels)
  if (is.null(frame)) {
    return(NULL)
  }
  tryCatch(
    stats::model.matrix(
      stats::delete.response(terms),
      frame,
      contrasts.arg = contrasts
    ),
    error = function(cnd) NULL
  )
}

synthetic_frame <- function(terms, xlevels) {
  if (length(xlevels) == 0) {
    return(NULL)
  }
  classes <- attr(terms, "dataClasses")
  vars <- unique(unlist(
    strsplit(attr(terms, "term.labels"), ":", fixed = TRUE)
  ))
  n <- max(2, lengths(xlevels))

  out <- map(vars, function(var) {
    if (!is.null(xlevels[[var]])) {
      return(factor(rep_len(xlevels[[var]], n), levels = xlevels[[var]]))
    }
    if (identical(unname(classes[var]), "logical")) {
      return(rep_len(c(TRUE, FALSE), n))
    }
    if (!identical(unname(classes[var]), "numeric")) {
      return(NULL)
    }
    as.numeric(seq_len(n))
  })
  if (any(map_lgl(out, is.null))) {
    return(NULL)
  }

  names(out) <- vars
  attr(out, "row.names") <- seq_len(n)
  class(out) <- "data.frame"
  out
}

# Work out the decomposition from the number of columns alone, for a model that
# recorded no levels at all.
#
# Every term made only of numeric predictors takes exactly one column, so with
# at most one factor in the formula the number of columns that factor expanded
# into is fixed, and the level each of those columns stands for is whatever
# follows the variable name. Anything less clear cut is left alone.
infer_term_fields <- function(terms, cols) {
  term_labels <- attr(terms, "term.labels")
  classes <- attr(terms, "dataClasses")
  n_terms <- length(term_labels)
  if (n_terms == 0 || length(cols) == 0) {
    return(NULL)
  }

  term_vars <- strsplit(term_labels, ":", fixed = TRUE)
  if (!all(unlist(term_vars) %in% names(classes))) {
    return(NULL)
  }
  # `ordered` is left out: its columns are named after the polynomial contrast,
  # not after the levels.
  cat_classes <- c("factor", "character", "logical")
  var_classes <- map(term_vars, ~ unname(classes[.x]))
  if (!all(map_lgl(var_classes, ~ all(.x %in% c("numeric", cat_classes))))) {
    return(NULL)
  }
  is_cat <- map_lgl(var_classes, ~ any(.x %in% cat_classes))

  sizes <- rep(1L, n_terms)
  cat_term <- which(is_cat)
  if (length(cat_term) > 1) {
    return(NULL)
  }
  if (length(cat_term) == 1) {
    if (length(term_vars[[cat_term]]) != 1) {
      return(NULL)
    }
    sizes[[cat_term]] <- length(cols) - (n_terms - 1L)
  }
  if (any(sizes < 1) || sum(sizes) != length(cols)) {
    return(NULL)
  }

  assign <- rep(seq_len(n_terms), sizes)
  out <- map2(cols, assign, function(col, term) {
    vars <- term_vars[[term]]
    if (!is_cat[[term]]) {
      return(map(vars, ~ list(type = "ordinary", col = .x)))
    }
    level <- substr(col, nchar(vars) + 1, nchar(col))
    if (!startsWith(col, vars) || level == "") {
      return(NULL)
    }
    list(list(type = "conditional", col = vars, val = level, op = "equal"))
  })
  if (any(map_lgl(out, is.null))) {
    return(NULL)
  }
  out
}

# Every way `label` can be read as the `:`-separated expansion of `vars`.
#
# Each element of the result is a field list of the same shape
# `parse_label_lm()` returns. More than one element means the label is
# genuinely ambiguous.
match_label_fields <- function(label, vars, xlevels, classes) {
  levels_of <- function(var) {
    if (!is.null(xlevels[[var]])) {
      return(xlevels[[var]])
    }
    # A logical predictor expands to a single `<var>TRUE` column and is not
    # recorded in `xlevels`.
    if (identical(unname(classes[var]), "logical")) {
      return(c("TRUE", "FALSE"))
    }
    NULL
  }

  step <- function(rest, i) {
    var <- vars[[i]]
    last <- i == length(vars)
    levels <- levels_of(var)
    out <- list()

    # Candidate pieces of `rest` that `var` could have produced, paired with
    # the field each implies.
    pieces <- list()
    if (is.null(levels)) {
      # A numeric predictor names its column after itself. It may carry a
      # suffix when it is a matrix column, so allow the piece to run to any
      # later `:` as well.
      ends <- unique(c(nchar(var), which(strsplit(rest, "")[[1]] == ":") - 1))
      ends <- ends[ends >= nchar(var) & ends <= nchar(rest)]
      for (end in ends) {
        piece <- substr(rest, 1, end)
        if (startsWith(piece, var)) {
          pieces <- c(
            pieces,
            list(list(
              piece = piece,
              field = list(type = "ordinary", col = piece)
            ))
          )
        }
      }
    } else {
      for (level in levels) {
        pieces <- c(
          pieces,
          list(list(
            piece = paste0(var, level),
            field = list(
              type = "conditional",
              col = var,
              val = level,
              op = "equal"
            )
          ))
        )
      }
    }

    for (piece in pieces) {
      if (!startsWith(rest, piece$piece)) {
        next
      }
      tail <- substr(rest, nchar(piece$piece) + 1, nchar(rest))
      if (last) {
        if (tail == "") {
          out <- c(out, list(list(piece$field)))
        }
      } else if (startsWith(tail, ":")) {
        for (sub in step(substr(tail, 2, nchar(tail)), i + 1)) {
          out <- c(out, list(c(list(piece$field), sub)))
        }
      }
    }
    out
  }

  step(label, 1)
}

parse_label_lm <- function(label, vars) {
  all_items <- NULL
  items <- strsplit(label, "\\:")[[1]]
  for (i in seq_len(length(items))) {
    item <- list(
      type = "ordinary",
      col = items[i]
    )
    cat_match <- map_lgl(vars, ~ .x == substr(items[i], 1, nchar(.x)))
    if (
      any(cat_match) &&
        any(vars[cat_match] != items[i]) &&
        !(items[i] %in% vars)
    ) {
      cat_match_vars <- vars[cat_match]
      sole_cat_match <- cat_match_vars[[which.max(nchar(cat_match_vars))]]
      item <- list(
        type = "conditional",
        col = sole_cat_match,
        val = substr(items[i], nchar(sole_cat_match) + 1, nchar(items[i])),
        op = "equal"
      )
    }
    all_items <- c(all_items, list(item))
  }
  all_items
}

parse_qr_lm <- function(label, qr) {
  if (is.null(qr)) {
    return(NULL)
  }
  qrs <- qr[label == rownames(qr)]
  qrs <- set_names(
    as.list(qrs),
    paste0("qr_", 1:length(qrs))
  )
}

# Intervals -----------------------------------------------

#' @export
tidypredict_interval.lm <- function(model, interval = 0.95) {
  parsedmodel <- parse_model(model)
  te_interval_lm(parsedmodel, interval)
}

get_qr_lm <- function(qr_name, parsedmodel) {
  q <- map(
    parsedmodel$terms,
    ~ {
      cqr <- .x$qr[qr_name][[1]]

      if (.x$is_intercept == 0) {
        cols <- map(.x$fields, lm_constructor)
        cols <- reduce_multiplication(cols)
        if (cqr != 0) {
          expr_multiplication(cols, cqr)
        }
      } else {
        expr(!!cqr)
      }
    }
  )
  f <- reduce_addition(q[!map_lgl(q, is.null)])

  expr(((!!f)) * ((!!f)) * !!parsedmodel$general$sigma2)
}

te_interval_lm <- function(
  parsedmodel,
  interval = 0.95,
  call = rlang::caller_env()
) {
  qr_names <- names(parsedmodel$terms[[1]]$qr)
  if (length(qr_names) == 0) {
    cli::cli_abort(
      c(
        x = "Unable to calculate the inverse of the QR decomposition.",
        i = "Prediction intervals are not available for this model, but
        {.fun tidypredict_fit} is."
      ),
      call = call
    )
  }
  qrs_map <- map(
    qr_names,
    ~ get_qr_lm(.x, parsedmodel)
  )
  qrs <- reduce_addition(qrs_map)
  # `sigma2` is added as a constant, which is the residual variance of an
  # unweighted observation.
  #
  # For a weighted fit `predict.lm()` scales it by the per-row `weights`
  # argument, but that argument defaults to `1` whenever `newdata` is given: it
  # warns and assumes a constant prediction variance, exactly as here. Since a
  # translated formula is always evaluated against new data, and nothing in it
  # names a weight column, matching that default is the only behavior available
  # and it is the one `predict()` itself takes.
  tfrac <- qt(1 - (1 - interval) / 2, parsedmodel$general$residual)
  expr(!!tfrac * sqrt((!!qrs) + (!!parsedmodel$general$sigma2)))
}

# Helpers -------------------------------------------------

lm_constructor <- function(x) {
  f <- NULL
  if (x$type == "ordinary") {
    f <- expr(!!as.name(x$col))
  }
  if (x$type == "conditional") {
    op <- x$op %||% "equal"
    f <- switch(
      op,
      "equal" = expr(ifelse(!!as.name(x$col) == !!x$val, 1, 0)),
      "not-equal" = expr(ifelse(!!as.name(x$col) != !!x$val, 1, 0)),
      "less" = expr(ifelse(!!as.name(x$col) < !!x$val, 1, 0)),
      "more-equal" = expr(ifelse(!!as.name(x$col) >= !!x$val, 1, 0)),
      cli::cli_abort("Operation {.val {op}} is not supported.")
    )
  }
  if (x$type == "operation") {
    if (x$op == "morethan") {
      f <- expr(ifelse(
        !!as.name(x$col) > !!x$val,
        !!as.name(x$col) - !!x$val,
        0
      ))
    }
    if (x$op == "lessthan") {
      f <- expr(ifelse(
        !!as.name(x$col) < !!x$val,
        !!x$val - !!as.name(x$col),
        0
      ))
    }
  }
  f
}
