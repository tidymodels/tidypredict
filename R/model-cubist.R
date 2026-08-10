#' @export
parse_model.cubist <- function(model) {
  coefs <- model$coefficients
  splits <- model$splits
  if (!is.null(splits)) {
    splits$variable <- as.character(splits$variable)
    splits$dir <- as.character(splits$dir)
  }

  # Pre-split data by committee and rule to avoid O(n) scans in nested loops
  coefs_by_comm_rule <- split(coefs, list(coefs$committee, coefs$rule))
  if (!is.null(splits)) {
    splits_by_comm_rule <- split(splits, list(splits$committee, splits$rule))
  }

  committees2 <- map(
    unique(coefs$committee),
    ~ {
      comm <- .x
      rules <- map(
        coefs$rule[coefs$committee == comm],
        ~ {
          key <- paste(comm, .x, sep = ".")
          cc <- coefs_by_comm_rule[[key]]
          if (!is.null(model$splits)) {
            cs <- splits_by_comm_rule[[key]]
            if (!is.null(cs) && nrow(cs) > 0) {
              tcs <- transpose(cs)
              mcs <- map(
                tcs,
                ~ list(
                  type = "conditional",
                  col = .x$variable,
                  # Cubist compares as a 32-bit float, so a value that rounds
                  # to the threshold belongs to the `<=` side.
                  val = f32_split_boundary(.x$value, "upper"),
                  op = ifelse(.x$dir == ">", "more", "less-equal")
                )
              )
            } else {
              mcs <- list(list(type = "all"))
            }
          } else {
            mcs <- list(list(type = "all"))
          }
          cc_names <- names(cc)
          f_coefs <- map(
            seq_along(cc_names),
            ~ {
              if (cc_names[.x] == "(Intercept)") {
                op <- "none"
                is_intercept <- 1
              } else {
                op <- "multiply"
                is_intercept <- 0
              }
              list(
                col = cc_names[.x],
                val = cc[, .x],
                op = op,
                is_intercept = is_intercept
              )
            }
          )

          f_na <- map_lgl(
            seq_along(cc_names),
            ~ !is.na(cc[, .x])
          )
          f_coefs <- f_coefs[f_na]
          f_coefs <- f_coefs[1:(length(f_coefs) - 2)]

          list(
            prediction = f_coefs,
            path = mcs
          )
        }
      )
    }
  )
  comm <- purrr::list_flatten(committees2)

  if (model$committees == 1) {
    ommittee_id <- rep(1, length(comm))
  } else {
    model_print <- utils::capture.output(print(model))
    model_print <- model_print[grep(
      "Number of rules per committee",
      model_print
    )]
    model_print <- regmatches(
      model_print,
      m = gregexpr("[0-9]+", model_print)
    )[[
      1
    ]]
    ommittee_id <- as.integer(model_print)
    ommittee_id <- rep(seq_along(ommittee_id), times = ommittee_id)
  }

  pm <- list(
    general = list(
      model = "cubist",
      type = "tree",
      version = 3,
      mode = "ifelse",
      n_committees = model$committees,
      ommittee_id = ommittee_id,
      means = cubist_attribute_means(model)
    ),
    trees = list(comm)
  )
  as_parsed_model(pm)
}

#' @export
tidypredict_fit.cubist <- function(model) {
  parsedmodel <- parse_model(model)
  tidypredict_fit_cubist(parsedmodel)
}

# The per-predictor training means `Cubist` writes into its model text.
#
# `Cubist` substitutes these for a missing value, so they are what
# `predict()` uses. They are stored at the model's own precision rather than
# recomputed from the training data, because the rounded value is the one the
# C code reads back and predicts from.
cubist_attribute_means <- function(model) {
  lines <- strsplit(model$model, "\n")[[1]]
  matches <- regmatches(
    lines,
    regexec('^att="([^"]+)" mean="([^"]+)"', lines)
  )
  matches <- Filter(\(x) length(x) == 3 && x[[2]] != "outcome", matches)

  means <- lapply(matches, \(x) as.numeric(x[[3]]))
  names(means) <- vapply(matches, \(x) x[[2]], character(1))
  means
}

# Replace every mention of a predictor with its training mean where the value
# is missing. `Cubist` does this for the rule conditions as well as the linear
# models, so the substitution is applied to the whole expression rather than to
# the coefficients alone.
substitute_missing_means <- function(x, means) {
  if (is.symbol(x)) {
    mean <- means[[as.character(x)]]
    if (is.null(mean)) {
      return(x)
    }
    return(expr(ifelse(is.na(!!x), !!mean, !!x)))
  }
  if (is.call(x)) {
    # Position 1 holds the function being called, not a column.
    for (i in seq_along(x)[-1]) {
      if (!identical(x[[i]], quote(expr = ))) {
        x[[i]] <- substitute_missing_means(x[[i]], means)
      }
    }
  }
  x
}

tidypredict_fit_cubist <- function(parsedmodel) {
  rules <- generate_tree_nodes(parsedmodel$trees[[1]], parsedmodel$general$mode)
  paths <- lapply(parsedmodel$trees[[1]], function(x) path_formulas(x$path))

  n_committees <- parsedmodel$general$n_committees
  ommittee_id <- parsedmodel$general$ommittee_id

  committees <- purrr::map2(
    split(rules, ommittee_id),
    split(paths, ommittee_id),
    make_committee
  )

  out <- reduce_addition(committees)
  if (n_committees > 1) {
    # Average the committes
    out <- expr_division(out, n_committees)
  }

  means <- parsedmodel$general$means
  if (length(means) > 0) {
    out <- substitute_missing_means(out, means)
  }

  out
}

make_committee <- function(rules, paths) {
  # cubist averages out rules if multiple apply
  paths <- lapply(paths, function(x) x %||% TRUE)
  paths <- reduce_addition(paths)
  rules <- reduce_addition(rules)
  if (identical(paths, TRUE)) {
    res <- rules
  } else {
    res <- expr_division(rules, paths)
  }
  res
}

build_tree_formula.pm_tree_cubist <- function(model) {
  tidypredict_fit_cubist(model)
}
