#' @export
tidypredict <- function(model){
  UseMethod("tidypredict")
}

#' @export
tidypredict.lm <- function(model){
  acceptable_formula(model)
  
  parsedmodel <- parsemodel(model)
  
  offset <- model$call$offset
  
  te_formula_lm(parsedmodel, offset)
}

#' @export
tidypredict.glm <- function(model){
  acceptable_formula(model)
  
  parsedmodel <- parsemodel(model)

  te_formula_lm(parsedmodel)
}