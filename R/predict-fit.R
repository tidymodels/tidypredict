#' Returns a Tidy Eval formula to calculate fitted values
#' 
#' It uses parsemodel to create a tabular version of the data needed to create
#' a Tidy Eval formula that can then be used inside a dplyr command.  It uses 
#' S3 methods to automatically determine the class of the model and run the
#' corresponding function that is able to create the Tidy Eval formula.  It
#' currently supports lm() and glm() models.
#' 
#' @param model An R model object
#'
#' @examples
#' 
#' df <- data.frame(x = c(1, 2, 5, 6 ,6), y = c(2, 3, 6, 5, 4))
#' model <- lm(x ~ y, df)
#' predict_fit(model)
#'
#' 
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
  parsedmodel <- parsemodel(model)
  te_fit_glm(parsedmodel)
}

#' @export
#' @importFrom tibble as.tibble
`predict_fit.data.frame` <- function(model){
  
  model <-  model %>% 
    mutate_if(is.factor, as.character) %>% 
    as.tibble()
  
  model_type <- model %>%
    filter(labels == "model") %>%
    pull(vals)
  
  assigned <- 0
  
  if(model_type == "lm"){
    assigned <- 1
    fit <- te_fit_lm(model)
  }
  
  if(model_type == "glm"){
    assigned <- 1
    fit <- te_fit_glm(model)
  }
  
  if(assigned ==0){
    stop("Model not recognized")
  }

  fit
}
