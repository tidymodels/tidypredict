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
