# Predict ---------------------------------------

# Parse model --------------------------------------

#' @export
parse_model.xrf <- function(model) parse_model_xrf(model)

parse_model_xrf <- function(model, call = rlang::caller_env()) {
  acceptable_formula(model)

  glmnet_fit <- model$glm$model$glmnet.fit
  if (inherits(glmnet_fit, "multnet")) {
    cli::cli_abort(
      c(
        "Multinomial {.pkg xrf} models are not supported.",
        i = "Only {.code family = \"gaussian\"} and
        {.code family = \"binomial\"} are supported."
      ),
      call = call
    )
  }

  # `rules::rule_fit()` records the tuned penalty on the fitted object, other-
  # wise fall back to the value `predict.xrf()` uses by default.
  lambda <- model$lambda %||% "lambda.min"
  coefs <- stats::coef(model$glm$model, s = lambda)
  coefs <- setNames(as.numeric(coefs), rownames(coefs))

  vars <- all.vars(stats::delete.response(stats::terms(model$base_formula)))
  xlev <- model$glm$xlev %||% list()
  rules <- model$rules
  feature_map <- xrf_feature_map(model)

  terms <- list()
  for (label in names(coefs)) {
    coef <- unname(coefs[[label]])
    if (coef == 0) {
      next
    }
    if (label == "(Intercept)") {
      fields <- list()
    } else if (label %in% rules$rule_id) {
      fields <- xrf_rule_fields(
        rules[rules$rule_id == label, ],
        feature_map,
        xlev,
        vars,
        call = call
      )
    } else {
      fields <- list(xrf_feature_field(
        label,
        feature_map,
        xlev,
        vars,
        call = call
      ))
    }
    terms[[length(terms) + 1]] <- list(
      label = label,
      coef = coef,
      is_intercept = as.integer(label == "(Intercept)"),
      fields = fields
    )
  }

  pm <- list()
  pm$general$model <- "xrf"
  pm$general$version <- 2
  pm$general$type <- "regression"
  pm$general$is_glm <- 1

  if (inherits(glmnet_fit, "lognet")) {
    pm$general$family <- "binomial"
    pm$general$link <- "logit"
  } else if (inherits(glmnet_fit, "elnet")) {
    pm$general$family <- "gaussian"
    pm$general$link <- "identity"
  } else {
    # nocov start
    cli::cli_abort(
      "Model fit with this {.arg family} is not supported.",
      .internal = TRUE
    )
  } # nocov end

  pm$terms <- terms
  as_parsed_model(pm)
}

# Fit model --------------------------------------
#
# The lasso can shrink every rule away, leaving an intercept-only formula that
# mentions no column, so anchor the result to a predictor. `all.vars()` on the
# base formula gives the raw columns rather than the model matrix names, so
# the anchor is a column of `newdata` even for a formula that transforms its
# predictors.
#' @export
tidypredict_fit.xrf <- function(model) {
  expr_recycle_over_column(
    tidypredict_fit(parse_model(model)),
    all.vars(stats::delete.response(stats::terms(model$base_formula)))
  )
}

# The names `xrf` gave the columns of its model matrix, paired with the column
# each one came from. Reading the mapping off a rebuilt model matrix settles
# cases that a name alone cannot, such as a `cyl4` dummy of a factor `cyl`
# sitting next to a separate column literally named `cyl4`.
#
# `NULL` when the model matrix cannot be rebuilt, in which case the names are
# all there is to go on.
xrf_feature_map <- function(model) {
  terms <- model$glm$formula
  xlev <- model$glm$xlev %||% list()
  labels <- attr(terms, "term.labels")
  if (is.null(terms) || length(labels) == 0 || length(xlev) == 0) {
    return(NULL)
  }

  n <- max(2, lengths(xlev))
  frame <- map(labels, function(var) {
    if (is.null(xlev[[var]])) {
      return(as.numeric(seq_len(n)))
    }
    factor(rep_len(xlev[[var]], n), levels = xlev[[var]])
  })
  names(frame) <- labels
  attr(frame, "row.names") <- seq_len(n)
  class(frame) <- "data.frame"

  mm <- tryCatch(
    stats::model.matrix(stats::delete.response(terms), frame),
    error = function(cnd) NULL
  )
  if (is.null(mm)) {
    return(NULL)
  }

  assign <- attr(mm, "assign")
  keep <- assign != 0
  cols <- colnames(mm)[keep]
  sources <- labels[assign[keep]]

  out <- map2(cols, sources, function(col, var) {
    if (is.null(xlev[[var]])) {
      return(list(type = "ordinary", col = var))
    }
    level <- substr(col, nchar(var) + 1, nchar(col))
    list(type = "conditional", col = var, val = level, op = "equal")
  })
  names(out) <- cols
  # Two columns of the model matrix sharing a name leaves the coefficients of
  # the fit genuinely ambiguous.
  out[cols %in% cols[duplicated(cols)]] <- list(NULL)
  out
}

# `xrf` fits the lasso on the columns of a model matrix, so a coefficient name
# is either a numeric column, a dummy column of a factor (or character) column,
# or the name of a rule.
xrf_feature_field <- function(
  feature,
  feature_map,
  xlev,
  vars,
  call = rlang::caller_env()
) {
  if (feature %in% names(feature_map)) {
    field <- feature_map[[feature]]
    if (is.null(field)) {
      cli::cli_abort(
        c(
          "Unable to tell which column the model term {.val {feature}}
          refers to.",
          i = "More than one column of the model matrix is named
          {.val {feature}}."
        ),
        call = call
      )
    }
    return(field)
  }

  if (feature %in% vars) {
    return(list(type = "ordinary", col = feature))
  }

  # Match the longest variable name first so that `x` and `x1` are not confused.
  cat_vars <- names(xlev)
  cat_vars <- cat_vars[order(-nchar(cat_vars))]
  for (var in cat_vars) {
    if (!startsWith(feature, var)) {
      next
    }
    level <- substr(feature, nchar(var) + 1, nchar(feature))
    if (level %in% xlev[[var]]) {
      return(list(type = "conditional", col = var, val = level, op = "equal"))
    }
  }

  cli::cli_abort(
    c(
      "Unable to map the model term {.val {feature}} to a column.",
      i = "Transformations and interactions in the formula are not supported."
    ),
    call = call
  )
}

# A rule is the intersection of its splits, which `build_linear_predictor()`
# turns into a product of indicators.
xrf_rule_fields <- function(
  rule,
  feature_map,
  xlev,
  vars,
  call = rlang::caller_env()
) {
  map(
    seq_len(nrow(rule)),
    function(i) {
      field <- xrf_feature_field(
        rule$feature[[i]],
        feature_map,
        xlev,
        vars,
        call = call
      )
      split <- rule$split[[i]]
      less_than <- rule$less_than[[i]]

      if (field$type == "ordinary") {
        field$type <- "conditional"
        field$val <- split
        field$op <- if (less_than) "less" else "more-equal"
        return(field)
      }

      # Splits on a dummy column separate the level from everything else. The
      # dummy is 0 or 1, so any threshold in (0, 1] gives the same partition.
      if (split <= 0 || split > 1) {
        # nocov start
        cli::cli_abort(
          "Unexpected split value {.val {split}} on a categorical column.",
          .internal = TRUE
        )
      } # nocov end
      field$op <- if (less_than) "not-equal" else "equal"
      field
    }
  )
}

#' @export
acceptable_formula.xrf <- function(model) {
  accepted_funs <- c("~", "+", "-", "*", "(", ")", ":", "::", "factor", "stats")
  funs <- fun_calls(model$base_formula)
  funs <- funs[!(funs %in% accepted_funs)]
  if (length(funs) > 0) {
    cli::cli_abort(
      c(
        x = "Functions inside the formula are not supported.",
        i = "Functions detected: {.val {funs}}.
            Use `dplyr` transformations to prepare the data."
      )
    )
  }
}

# Test --------------------------------------------

#' @export
tidypredict_test.xrf <- function(
  model,
  df,
  threshold = 0.000000000001,
  include_intervals = FALSE,
  max_rows = NULL,
  xg_df = NULL
) {
  if (is.numeric(max_rows)) {
    df <- head(df, max_rows) # nocov
  }

  lambda <- model$lambda %||% "lambda.min"
  preds <- predict(model, df, lambda = lambda, type = "response")
  base <- data.frame(fit = as.vector(preds), row.names = NULL)

  te <- tidypredict_to_column(
    df,
    model,
    add_interval = FALSE,
    vars = c("fit_te", "upr_te", "lwr_te")
  )
  test_results_numeric(
    base$fit,
    te[, "fit_te"],
    threshold,
    model$base_formula
  )
}
