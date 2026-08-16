# Predict ---------------------------------------

# Parse model --------------------------------------

#' @export
parse_model.sda <- function(model) parse_model_sda(model)

parse_model_sda <- function(model, vars = character(0), preproc = NULL) {
  # `predict.sda()` computes `Xtest %*% t(beta) + alpha` and takes the softmax of
  # the result, so each class is already a plain linear predictor. Only the
  # features that survived shrinkage appear in `beta`.
  classes <- names(model$alpha)
  labels <- c("(Intercept)", colnames(model$beta))
  fields <- preproc_fields(labels, preproc)

  class_terms <- lapply(seq_along(classes), function(i) {
    build_terms(
      c(model$alpha[[i]], model$beta[i, ]),
      labels,
      vars,
      fields = fields
    )
  })

  new_multiclass_parsed_model(
    "sda",
    classes,
    class_terms
  )
}


# Test ---------------------------------------------

#' @export
tidypredict_test.sda <- function(
  model,
  df,
  threshold = 0.000000000001,
  include_intervals = FALSE,
  max_rows = NULL,
  xg_df = NULL
) {
  abort_test_unsupported("{.fn sda::sda} models")
}
