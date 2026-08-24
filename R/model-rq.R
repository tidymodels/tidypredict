# Predict ---------------------------------------

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

# Output metadata ---------------------------------

# `parse_model.rqs()` returns a bare list of parsed models rather than a single
# parsed model, so the default cannot route through it. Several `tau` give a
# named list of expressions that looks exactly like a multiclass probability
# list but holds unrelated quantile predictions.
#' @export
tidypredict_output_type.rqs <- function(x, ...) {
  rlang::check_dots_empty()
  "numeric"
}

#' @export
tidypredict_outcome_levels.rqs <- function(x, ...) {
  rlang::check_dots_empty()
  NULL
}

#' @export
tidypredict_normalized.rqs <- function(x, ...) {
  rlang::check_dots_empty()

  # The names are quantiles, not levels, and the values do not sum to anything.
  NA
}
