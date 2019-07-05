#' @export
tidypredict_fit.model_fit <- function(model) {
  tidypredict_fit(model$fit)
}

#' @export
parse_model.model_fit <- function(model) {
  parse_model(model$fit)
}
