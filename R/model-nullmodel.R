# Predict ---------------------------------------

#' @export
tidypredict_fit.nullmodel <- function(model) {
  tidypredict_fit(parse_model(model))
}

#' @export
tidypredict_fit.pm_nullmodel_classification <- function(model) {
  set_names(map(model$class_probs, ~ expr(!!.x)), model$classes)
}

# Parse model --------------------------------------

#' @export
parse_model.nullmodel <- function(model) {
  # `nullmodel()` ignores the predictors entirely: it stores the mean of the
  # outcome for regression, and the most frequent level plus the observed class
  # frequencies for classification. Predictions are therefore constants.
  if (is.null(model$levels)) {
    return(parse_model_nullmodel_regression(model))
  }
  parse_model_nullmodel_classification(model)
}

parse_model_nullmodel_regression <- function(model) {
  pm <- list()
  pm$general$model <- "nullmodel"
  pm$general$version <- 2
  pm$general$type <- "regression"
  pm$general$is_glm <- 0
  pm$terms <- list(
    list(
      label = "(Intercept)",
      coef = as.numeric(model$value),
      is_intercept = 1L,
      fields = list(list(type = "ordinary", col = "(Intercept)"))
    )
  )

  as_parsed_model(pm)
}

parse_model_nullmodel_classification <- function(model) {
  classes <- as.character(model$levels)
  # `predict.nullmodel(type = "prob")` returns `pct`, which is only populated
  # for the levels seen during fitting. Unseen levels get a probability of 0.
  probs <- rep(0, length(classes))
  names(probs) <- classes
  pct <- model$pct
  probs[names(pct)] <- as.numeric(pct)

  pm <- list()
  pm$general$model <- "nullmodel"
  pm$general$version <- 2
  pm$general$type <- "nullmodel_classification"
  pm$classes <- classes
  pm$class_probs <- as.list(unname(probs))

  as_parsed_model(pm)
}

# Test ---------------------------------------------

#' @export
tidypredict_test.nullmodel <- function(
  model,
  df,
  threshold = 0.000000000001,
  include_intervals = FALSE,
  max_rows = NULL,
  xg_df = NULL
) {
  if (!is.null(model$levels)) {
    abort_test_unsupported(
      "classification {.fn parsnip::nullmodel} models",
      "class probabilities"
    )
  }

  tidypredict_test_default(
    model = model,
    df = df,
    threshold = threshold,
    include_intervals = include_intervals,
    max_rows = max_rows,
    xg_df = xg_df
  )
}
