# Predict ---------------------------------------

# Parse model --------------------------------------

#' @export
parse_model.lda <- function(model) {
  # `lda()` can also be fit with an `x`/`grouping` interface, in which case
  # there is no formula to check and every column is used as-is.
  vars <- character(0)
  if (!is.null(model$terms)) {
    acceptable_formula(model)
    vars <- names(attr(model$terms, "dataClasses"))
  }

  classes <- model$lev
  prior <- model$prior

  # `predict.lda()` centers the model matrix at the prior-weighted grand mean,
  # projects it onto the discriminant space, and then compares the result to
  # each class centroid. Because that whole path is linear in the predictors,
  # it collapses into one coefficient vector plus one intercept per class, and
  # the posterior probabilities are the softmax of those linear predictors.
  center <- colSums(prior * model$means)
  centroids <- scale(model$means, center = center, scale = FALSE) %*%
    model$scaling
  coefs <- model$scaling %*% t(centroids)
  intercepts <- -as.vector(center %*% coefs) -
    0.5 * rowSums(centroids^2) +
    log(prior)

  labels <- rownames(coefs)
  class_terms <- lapply(seq_along(classes), function(i) {
    build_terms(
      c(intercepts[[i]], coefs[, i]),
      c("(Intercept)", labels),
      vars
    )
  })

  new_multiclass_parsed_model(
    "lda",
    classes,
    class_terms
  )
}


#' @export
acceptable_formula.lda <- function(model) acceptable_lm(model)

# Test ---------------------------------------------

#' @export
tidypredict_test.lda <- function(
  model,
  df,
  threshold = 0.000000000001,
  include_intervals = FALSE,
  max_rows = NULL,
  xg_df = NULL
) {
  abort_test_unsupported("{.fn MASS::lda} models")
}
