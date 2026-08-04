# Predict ---------------------------------------

#' @export
tidypredict_fit.qda <- function(model) {
  parsedmodel <- parse_model(model)
  build_fit_formula_multinom(parsedmodel)
}

# Parse model --------------------------------------

#' @export
parse_model.qda <- function(model) {
  # `qda()` can also be fit with an `x`/`grouping` interface, in which case
  # there is no formula to check and every column is used as-is.
  vars <- character(0)
  if (!is.null(model$terms)) {
    acceptable_formula(model)
    vars <- names(attr(model$terms, "dataClasses"))
  }

  classes <- model$lev
  prior <- model$prior
  labels <- colnames(model$means)
  p <- length(labels)

  # `predict.qda()` scores each class with
  # `-0.5 * ||(x - m_k) S_k||^2 - 0.5 * ldet_k + log(prior_k)` and turns those
  # scores into posterior probabilities with a softmax. With `A_k = S_k S_k'`
  # the squared norm expands to `x' A_k x - 2 m_k' A_k x + m_k' A_k m_k`, so
  # each class score is a quadratic polynomial: one intercept, one coefficient
  # per predictor, and one coefficient per pair of predictors.
  class_terms <- lapply(seq_along(classes), function(i) {
    a <- tcrossprod(matrix(model$scaling[,, i], nrow = p, ncol = p))
    means <- model$means[i, ]

    linear <- as.vector(means %*% a)
    intercept <- -0.5 *
      sum(linear * means) -
      0.5 * model$ldet[[i]] +
      log(prior[[i]])

    pairs <- expand_quadratic(a, labels)

    build_terms(
      c(intercept, linear, pairs$coefs),
      c("(Intercept)", labels, pairs$labels),
      vars
    )
  })

  new_multiclass_parsed_model(
    "qda",
    classes,
    class_terms
  )
}

# Coefficients of `-0.5 * x' A x`, one per unique pair of predictors. The
# off-diagonal entries are counted twice in the quadratic form, so they lose
# the halving.
expand_quadratic <- function(a, labels) {
  coefs <- numeric(0)
  pair_labels <- character(0)
  for (j in seq_along(labels)) {
    for (k in j:length(labels)) {
      coefs <- c(coefs, if (j == k) -0.5 * a[j, k] else -a[j, k])
      pair_labels <- c(pair_labels, paste0(labels[j], ":", labels[k]))
    }
  }
  list(coefs = coefs, labels = pair_labels)
}

#' @export
acceptable_formula.qda <- function(model) acceptable_lm(model)

# Test ---------------------------------------------

#' @export
tidypredict_test.qda <- function(
  model,
  df,
  threshold = 0.000000000001,
  include_intervals = FALSE,
  max_rows = NULL,
  xg_df = NULL
) {
  abort_test_unsupported("{.fn MASS::qda} models")
}
