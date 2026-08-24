# Predict ---------------------------------------

# Parse model --------------------------------------

#' @export
parse_model.glm <- function(model) parse_model_lm(model)

# Intervals -----------------------------------------------

#' @export
tidypredict_interval.glm <- function(model, interval = 0.95) {
  check_interval(interval)
  parsedmodel <- parse_model(model)
  te_interval_glm(parsedmodel, interval)
}

te_interval_glm <- function(parsedmodel, interval = 0.95) {
  intervals <- te_interval_lm(parsedmodel, interval)
  family <- parsedmodel$general$family
  link <- parsedmodel$general$link
  assigned <- 0
  if (family == "gaussian" && link == "identity") {
    assigned <- 1
  }
  if (assigned == 0) {
    cli::cli_abort(
      "Combination of family and link are not supported for prediction intervals."
    )
  }
  intervals
}

# Output metadata ---------------------------------

# The parsed form is coefficients only, so the response levels have to come off
# the fitted object. A binomial glm fit on a 0/1 numeric records none, which is
# the "did not retain the levels" case.
#' @export
tidypredict_outcome_levels.glm <- function(x, ...) {
  rlang::check_dots_empty()

  if (!identical(x$family$family, "binomial")) {
    return(NULL)
  }

  response <- stats::model.frame(x)[[1]]
  if (!is.factor(response)) {
    return(NULL)
  }
  levels(response)
}
