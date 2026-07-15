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
  models <- split_rqs(model)
  set_names(
    map(models, tidypredict_fit),
    paste0("quantile_", format(model$tau))
  )
}

#' @export
parse_model.rqs <- function(model) {
  models <- split_rqs(model)
  set_names(
    map(models, parse_model),
    paste0("quantile_", format(model$tau))
  )
}

#' @export
acceptable_formula.rqs <- function(model) {
  acceptable_lm(model)
}

# Split an `rqs` object (multiple quantiles) into a list of single-quantile
# `rq` objects, one per column of the coefficient matrix.
split_rqs <- function(model) {
  coefs <- model$coefficients
  map(
    seq_along(model$tau),
    ~ {
      one <- model
      class(one) <- "rq"
      one$coefficients <- coefs[, .x]
      one$tau <- model$tau[.x]
      one
    }
  )
}
