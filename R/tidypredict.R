#' @export
predict_fit <- function(model){
  UseMethod("predict_fit")
}

#' @export
predict_fit.lm <- function(model){
  
  parsedmodel <- parsemodel(model)
  
  te_fit_lm(parsedmodel)
}

#' @export
predict_fit.glm <- function(model){
  predict_fit.lm(model)
}

#' @export
predict_interval <- function(model, interval = 0.95){
  UseMethod("predict_interval")
}

#' @export
predict_interval.lm <- function(model, interval = 0.95){

  parsedmodel <- parsemodel(model)
  
  te_interval_lm (parsedmodel, interval)
}

#' @export
predict_interval.glm <- function(model, interval = 0.95){
  predict_interval.lm(model, interval)
}

