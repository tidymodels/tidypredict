# Formula builder ---------------------------------------------------
te_fit_glm <- function(parsedmodel) {
  fit <- te_fit_lm(parsedmodel)
  
  family <- pull(filter(parsedmodel, labels == "family"), .data$vals)
  link <- pull(filter(parsedmodel, labels == "link"), .data$vals)
  
  assigned <- 0
  
  if (link == "identity") {
    assigned <- 1
  }
  
  if (link == "logit") {
    assigned <- 1
    fit <- expr(1 - 1 / (1 + exp(!! fit)))
  }
  
  if (link == "log") {
    assigned <- 1
    fit <- expr(exp(!! fit))
  }
  
  if (assigned == 0) {
    stop("Combination of family and link are not supported")
  }
  
  fit
}

te_interval_glm <- function(parsedmodel, interval = 0.95) {
  intervals <- te_interval_lm(parsedmodel, interval)
  
  family <- pull(filter(parsedmodel, .data$labels == "family"), .data$vals)
  link <- pull(filter(parsedmodel, .data$labels == "link"), .data$vals)
  
  assigned <- 0
  
  if (family == "gaussian" && link == "identity") {
    assigned <- 1
  }
  
  if (assigned == 0) {
    stop("Combination of family and link are not supported for prediction intervals")
  }
  
  intervals
}

# Model parser ------------------------------------------------------
#' @export
parse_model.glm <- function(model) parse_model_lm(model)

#' @export
tidypredict_interval.glm <- function(model, interval = 0.95) {
  parsedmodel <- parse_model(model)
  te_interval_glm(parsedmodel, interval)
}

# Fit method ------------------------------------------------------
#' @export
tidypredict_fit.glm <- function(model) {
  parsedmodel <- parse_model(model)
  te_fit_glm(parsedmodel)
}

#' @export
acceptable_formula.glm <- function(model) {
  acceptable_lm(model)
}
