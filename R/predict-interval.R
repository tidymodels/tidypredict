#' Returns a Tidy Eval formula to calculate prediction interval
#'
#' It parses a model or uses an already parsed model to return a
#' Tidy Eval formula that can then be used inside a dplyr command.
#'
#' The result still has to be added to the fit to obtain the upper bound, and
#' subtracted from fit to obtain the lower bound.
#'
#' @param model An R model or a tibble with a parsed model. It currently supports
#' lm() models only.
#' @param interval The prediction interval, defaults to 0.95
#'
#'
#' @export
tidypredict_interval <- function(model, interval = 0.95) {
  UseMethod("tidypredict_interval")
}

#' @export
tidypredict_interval.lm <- function(model, interval = 0.95) {
  parsedmodel <- parse_model(model)
  te_interval_lm(parsedmodel, interval)
}

#' @export
tidypredict_interval.glm <- function(model, interval = 0.95) {
  parsedmodel <- parse_model(model)
  te_interval_glm(parsedmodel, interval)
}

#' @export
`tidypredict_interval.data.frame` <- function(model, interval = 0.95) {
  model_type <- model[model$labels == "model", "vals"][[1]]
  
  assigned <- 0
  
  if (model_type == "lm") {
    assigned <- 1
    ret <- te_interval_lm(model, interval = interval)
  }
  
  if (model_type == "glm") {
    assigned <- 1
    ret <- te_interval_glm(model, interval = interval)
  }
  
  if (assigned == 0) {
    stop("Model not recognized")
  } else {
    ret
  }
}
