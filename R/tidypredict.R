#' @export
predict_fit <- function(model){
  UseMethod("predict_fit")
}

#' @export
predict_fit.lm <- function(model){
  acceptable_formula(model)
  
  parsedmodel <- parsemodel(model)
  
  offset <- model$call$offset
  
  te_fit_lm(parsedmodel, offset)
}

#' @export
predict_fit.glm <- function(model){
  acceptable_formula(model)
  
  parsedmodel <- parsemodel(model)

  te_fit_lm(parsedmodel)
}

#' @export
predict_interval <- function(model, interval = 0.95){
  UseMethod("predict_interval")
}

#' @export
predict_interval.lm <- function(model, interval = 0.95){
  
  acceptable_formula(model)
  
  te_interval_lm (model, interval)
}

#' @export
predict_interval.glm <- function(model, interval = 0.95){
  predict_interval.lm(model)
}

