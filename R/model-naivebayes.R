# Predict ---------------------------------------

#' @export
tidypredict_fit.NaiveBayes <- function(model) {
  parsedmodel <- parse_model(model)
  build_fit_formula_naive_bayes(parsedmodel)
}

#' @export
tidypredict_fit.naive_bayes <- function(model) {
  parsedmodel <- parse_model(model)
  build_fit_formula_naive_bayes(parsedmodel)
}

#' @export
tidypredict_fit.pm_naive_bayes <- function(model) {
  build_fit_formula_naive_bayes(model)
}

# `predict.NaiveBayes()` multiplies the class prior by one conditional density
# per predictor and normalizes the result, which is the softmax of the summed
# log densities. The `1 / sqrt(2 * pi)` factor of the normal densities is the
# same for every class, so it cancels out and is left out of the scores.
build_fit_formula_naive_bayes <- function(parsedmodel) {
  scores <- map(parsedmodel$class_terms, naive_bayes_score)
  expr_softmax(scores, parsedmodel$classes)
}

naive_bayes_score <- function(class_term) {
  terms <- map(class_term$terms, naive_bayes_term)
  reduce_addition(c(list(expr(!!class_term$intercept)), terms))
}

naive_bayes_term <- function(term) {
  col <- sym(term$var)

  if (term$type == "numeric") {
    # log(dnorm(x, mean, sd)), without the constant `-log(sqrt(2 * pi))`
    mean <- as.numeric(term$mean)
    scale <- as.numeric(term$scale)
    offset <- as.numeric(term$offset)
    return(skip_when_missing(
      col,
      expr(!!offset - ((!!col - !!mean)^2 / !!scale))
    ))
  }

  if (term$type == "poisson") {
    # log(dpois(x, lambda)), without the constant `-log(factorial(x))`
    log_lambda <- as.numeric(term$log_lambda)
    lambda <- as.numeric(term$lambda)
    return(skip_when_missing(col, expr(!!col * !!log_lambda - !!lambda)))
  }

  if (term$type == "logical") {
    return(expr(case_when(
      is.na(!!col) ~ 0,
      !!col ~ !!as.numeric(term$log_true),
      .default = !!as.numeric(term$log_false)
    )))
  }

  levels <- as.character(unlist(term$levels))
  log_probs <- as.numeric(unlist(term$log_probs))
  conditions <- map2(
    levels,
    log_probs,
    function(level, log_prob) expr(!!col == !!level ~ !!log_prob)
  )
  # A level that was not seen while fitting has no probability, and both
  # references skip such a feature rather than failing the whole row: `klaR`
  # coerces it to `NA` and returns a density of 1 for it, `naivebayes` sets the
  # density to 1 directly. A log density of 0 is a density of 1.
  expr(case_when(!!!conditions, .default = 0))
}

# Both references skip a feature whose value is missing rather than letting the
# whole row go missing, so the row falls back on the remaining features and, if
# every feature is missing, on the class prior alone. A log density of 0 is a
# density of 1, which leaves the summed score untouched.
#
# The test is on the column rather than on the term, so that an `NA` coming
# from a degenerate fitted parameter still propagates.
skip_when_missing <- function(col, term) {
  expr(ifelse(is.na(!!col), 0, !!term))
}

# Parse model --------------------------------------

#' @export
parse_model.NaiveBayes <- function(model) {
  if (isTRUE(model$usekernel)) {
    cli::cli_abort(
      c(
        "{.fn tidypredict_fit} does not support {.fn klaR::NaiveBayes} models
         fit with kernel density estimates.",
        "i" = "Refit with {.code usekernel = FALSE}."
      )
    )
  }

  classes <- model$levels
  # `predict.NaiveBayes()` replaces zero densities with `threshold`
  threshold <- 0.001

  class_terms <- lapply(seq_along(classes), function(i) {
    terms <- lapply(model$varnames, function(var) {
      naive_bayes_var(model, var, i, threshold)
    })
    list(
      intercept = log(as.numeric(model$apriori[[i]])),
      terms = terms
    )
  })

  pm <- list()
  pm$general$model <- "NaiveBayes"
  pm$general$version <- 2
  pm$general$type <- "naive_bayes"
  pm$general$threshold <- threshold
  pm$classes <- classes
  pm$class_terms <- class_terms

  as_parsed_model(pm)
}

naive_bayes_var <- function(model, var, i, threshold) {
  tbl <- model$tables[[var]]

  if (is.numeric(model$x[[var]])) {
    mean <- as.numeric(tbl[i, 1])
    sd <- as.numeric(tbl[i, 2])
    return(list(
      var = var,
      type = "numeric",
      mean = mean,
      scale = 2 * sd^2,
      offset = -log(sd)
    ))
  }

  probs <- as.numeric(tbl[i, ])
  probs[probs == 0] <- threshold

  if (is.logical(model$x[[var]])) {
    return(list(
      var = var,
      type = "logical",
      log_false = log(probs[[1]]),
      log_true = log(probs[[2]])
    ))
  }

  list(
    var = var,
    type = "factor",
    levels = colnames(tbl),
    log_probs = log(probs)
  )
}

#' @export
parse_model.naive_bayes <- function(model) {
  cond_dist <- attr(model$tables, "cond_dist")

  if (any(cond_dist == "KDE")) {
    cli::cli_abort(
      c(
        "{.fn tidypredict_fit} does not support {.fn naivebayes::naive_bayes}
         models fit with kernel density estimates.",
        "i" = "Refit with {.code usekernel = FALSE}."
      )
    )
  }

  classes <- model$levels
  # `predict.naive_bayes()` replaces zero densities with `threshold`
  threshold <- 0.001

  class_terms <- lapply(seq_along(classes), function(i) {
    terms <- lapply(names(model$tables), function(var) {
      naive_bayes_var_nb(model, var, i, cond_dist[[var]], threshold)
    })
    list(
      intercept = log(as.numeric(model$prior[[i]])),
      terms = terms
    )
  })

  pm <- list()
  pm$general$model <- "naive_bayes"
  pm$general$version <- 2
  pm$general$type <- "naive_bayes"
  pm$general$threshold <- threshold
  pm$classes <- classes
  pm$class_terms <- class_terms

  as_parsed_model(pm)
}

naive_bayes_var_nb <- function(model, var, i, cond_dist, threshold) {
  tbl <- model$tables[[var]]

  if (cond_dist == "Gaussian") {
    mean <- as.numeric(tbl[1, i])
    sd <- as.numeric(tbl[2, i])
    # A class with fewer than two observations has no standard deviation.
    # `predict()` returns `NA` probabilities for every row of such a fit, so the
    # `NA` is carried through rather than being treated as a zero variance.
    if (!is.na(sd) && sd <= 0) {
      sd <- threshold
    }
    return(list(
      var = var,
      type = "numeric",
      mean = mean,
      scale = 2 * sd^2,
      offset = -log(sd)
    ))
  }

  if (cond_dist == "Poisson") {
    lambda <- as.numeric(tbl[1, i])
    return(list(
      var = var,
      type = "poisson",
      lambda = lambda,
      log_lambda = log(lambda)
    ))
  }

  probs <- as.numeric(tbl[, i])
  if (model$laplace == 0) {
    probs[probs <= 0] <- threshold
  }

  if (is.logical(model$data$x[[var]])) {
    return(list(
      var = var,
      type = "logical",
      log_false = log(probs[[1]]),
      log_true = log(probs[[2]])
    ))
  }

  list(
    var = var,
    type = "factor",
    levels = rownames(tbl),
    log_probs = log(probs)
  )
}

# Test ---------------------------------------------

#' @export
tidypredict_test.NaiveBayes <- function(
  model,
  df,
  threshold = 0.000000000001,
  include_intervals = FALSE,
  max_rows = NULL,
  xg_df = NULL
) {
  abort_test_unsupported("{.fn klaR::NaiveBayes} models")
}

#' @export
tidypredict_test.naive_bayes <- function(
  model,
  df,
  threshold = 0.000000000001,
  include_intervals = FALSE,
  max_rows = NULL,
  xg_df = NULL
) {
  abort_test_unsupported("{.fn naivebayes::naive_bayes} models")
}
