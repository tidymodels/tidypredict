#' Adds the prediction columns to a piped command set
#'
#' Adds a new column with the results form tidypredict_fit() to a piped command set.
#' If add_interval is set to TRUE, then it will add two additional columns, one
#' for the lower and another for the upper prediction interval bounds.
#'
#' @param df A data.frame or tibble
#' @param model An R model or a parsed model inside a data frame
#' @param add_interval Switch that indicates if the prediction interval columns should be added. Defaults
#' to FALSE
#' @param interval The prediction interval, defaults to 0.95. It is ignored if add_interval is set to
#' FALSE
#' @param vars The name of the variables that this function will produce. It defaults to "fit", "upper", and "lower".
#' @param id_field For models with multiple predictions (such as trees).  The final prediction is a result of
#' grouping all of the predictions by each observation.  For performance and reproducibility reasons, it is better to
#' have a unique identifier for each observation (row).
#' @param ... Using dots for future argument expansion.
#'
#' @examples
#' library(dplyr)
#' df <- mutate(mtcars, cyl = paste0("cyl", cyl))
#' model <- lm(mpg ~ wt + cyl * disp, offset = am, data = df)
#'
#' df %>%
#'   tidypredict_to_column(model, add_interval = TRUE)
#' @export
tidypredict_to_column <- function(df, model, add_interval = FALSE,
                                  interval = 0.95, vars = c("fit", "upper", "lower"),
                                  id_field = NULL, ...) {
  id_field <- rlang::enexpr(id_field)
  
  fit <- vars[1]
  upper <- vars[2]
  lower <- vars[3]
  
  matched <- FALSE
  
  if(class(model) == "ranger"){
    matched <- TRUE
    preds <- tidypredict_fit(model)
    pred_type <- class(model$predictions)
    all <- NULL
    for(i in seq_along(preds) ){
      new <- mutate(df, fit = !! preds[[i]])
      if(is.null(all)){
        all <- new
      } else {
        all <- union_all(all, new)  
      }
    }
    if(pred_type == "factor"){
      if(is.null(id_field)){
        all <- group_by(all, !!! syms(c(colnames(df), "fit")))  
      } else {
        
        all <- group_by(all, !! id_field, fit)  
      }
      all <- tally(all)
      all <- filter(all, n == max(n, na.rm = TRUE))
      all <- select(all, -n)
      df <- ungroup(all)
    }
    if(pred_type == "numeric"){
      if(is.null(id_field)){
        all <- group_by(all, !!! syms(colnames(df)))
      } else {
        
        all <- group_by(all, !! id_field)  
      }
      all <- summarise(all, fit = mean(fit, na.rm = FALSE))
      df <- ungroup(all)
    }
  }
  
  if(matched == FALSE){
    df <- mutate(df, !! fit := !! tidypredict_fit(model))
    
    if (add_interval) {
      formulas <- c(sym(fit), tidypredict_interval(model, interval = interval))
      upper_formula <- reduce(formulas, function(l, r) expr((!! l) + (!! r)))
      lower_formula <- reduce(formulas, function(l, r) expr((!! l) - (!! r)))
      
      df <- mutate(
        df,
        !! upper := !! upper_formula,
        !! lower := !! lower_formula
      )
    }
  }

  df
}
