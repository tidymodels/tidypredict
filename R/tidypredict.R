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

#' Returns a Tidy Eval formula to calculate prediction interval
#' 
#' It uses parsemodel to create a tabular version of the data needed to create
#' a Tidy Eval formula that can then be used inside a dplyr command.  It uses 
#' S3 methods to automatically determine the class of the model and run the
#' corresponding function that is able to create the Tidy Eval formula.  It
#' currently supports lm() models.
#' 
#' The result still has to be added to the fit to obtain the upper bound, and
#' substracted from fit to obtain the lower bound.
#' 
#' @param model An R model object
#' @param interval The prediction interval, defaults to 0.95
#'
#' @examples
#' 
#' df <- data.frame(x = c(1, 2, 5, 6 ,6), y = c(2, 3, 6, 5, 4))
#' model <- lm(x ~ y, df)
#' predict_interval(model)
#'
#' 
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
  parsedmodel <- parsemodel(model)
  te_interval_glm (parsedmodel, interval)
}

#' @export
`predict_interval.data.frame` <- function(model, interval = 0.95){
  
  model <-  model %>% 
    mutate_if(is.factor, as.character) %>% 
    as.tibble()
  
  model_type <- model %>%
    filter(labels == "model") %>%
    pull(vals)

  assigned <- 0
  
  if(model_type == "lm"){
    assigned <- 1
    te_interval_lm(model)
  }
  
  if(model_type == "glm"){
    assigned <- 1
    te_interval_glm(model)
  }
  
  if(assigned ==0){
    stop("Model not recognized")
  }
}

#' Adds the prediction columns to a piped command set
#' 
#' Adds a new column with the results form predict_fit() to a piped command set.
#' If add_interval is set to TRUE, then it will add two additional columns, one 
#' for the lower and another for the upper prediction interval bounds.  
#' 
#' 
#' @param df A data.frame or tibble
#' @param model An R object, typically an R model, but it can also bee a regular data frame 
#' that has a parsed model already loaded.
#' @param add_interval Switch that indicates if the prediction interval columns should be added. Defaults
#' to FALSE
#' @param interval The prediction interval, defaults to 0.95. It is ignored if add_interval is set to
#' FALSE
#' @param vars The name of the variables that this function will produce. It defaults to "fit", "upper", and "lower".
#'
#' @examples
#' 
#' library(dplyr)
#' 
#' df <- data.frame(x = c(1, 2, 5, 6 ,6), y = c(2, 3, 6, 5, 4))
#' model <- lm(x ~ y, df)
#' df %>%
#'   predict_to_column(model, add_interval = TRUE)
#'
#' @import rlang
#' @importFrom purrr reduce
#' @import dplyr
#' @export
predict_to_column <- function(df, model, add_interval = FALSE, interval = 0.95, vars = c("fit", "upper", "lower")){
  
  fit <- vars[1]
  upper <- vars[2]
  lower <- vars[3]
  
  df <- mutate(df, !! fit := !! predict_fit(model))
  
  if(add_interval){
    
    formulas <- c(sym(fit) , predict_interval(model, interval = interval))
    upper_formula <- reduce(formulas, function(l, r) expr((!!l) + (!!r)))
    lower_formula <- reduce(formulas, function(l, r) expr((!!l) - (!!r)))
    
    df <- mutate(df, 
                 !! upper := !! upper_formula,
                 !! lower := !! lower_formula)
  }
  
  df
}
