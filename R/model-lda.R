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

  # `lev` keeps every level of the outcome factor, but `lda()` drops the levels
  # no observation fell in, so `prior`, `means` and the posterior returned by
  # `predict()` only cover the groups that were actually fit.
  prior <- model$prior
  classes <- names(prior)

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
  fields <- lm_fields(model, labels)
  if (!is.null(fields)) {
    fields <- c(list(NULL), fields)
  }
  class_terms <- lapply(seq_along(classes), function(i) {
    build_terms(
      c(intercepts[[i]], coefs[, i]),
      c("(Intercept)", labels),
      vars,
      fields = fields
    )
  })

  new_multiclass_parsed_model(
    "lda",
    classes,
    class_terms
  )
}


#' @export
acceptable_formula.lda <- function(model) {
  # `lda()` records no `contrasts` for `acceptable_lm()` to read, so they are
  # read back off the names it gave the columns each factor expanded into.
  acceptable_contrasts(
    columns = colnames(model$means),
    vars = names(attr(model$terms, "dataClasses")),
    xlevels = model$xlevels
  )

  acceptable_lm(model)
}

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
