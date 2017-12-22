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