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

# rqs (multiple quantiles) -------------------------

#' @export
tidypredict_fit.rqs <- function(model) {
  cli::cli_abort(
    c(
      "{.pkg tidypredict} does not support {.fn quantreg::rq} models fitted \\
      with more than one quantile ({.arg tau}).",
      i = "Fit a separate model for each quantile level instead."
    )
  )
}

#' @export
parse_model.rqs <- function(model) tidypredict_fit.rqs(model)
