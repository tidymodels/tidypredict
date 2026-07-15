# Predict ---------------------------------------

#' @export
tidypredict_fit.rq <- function(model) {
  parsedmodel <- parse_model(model)
  build_fit_formula(parsedmodel)
}

# Parse model --------------------------------------

#' @export
parse_model.rq <- function(model) parse_model_lm(model)

#' @export
acceptable_formula.rq <- function(model) {
  acceptable_lm(model)
}
