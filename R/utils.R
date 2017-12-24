test_predictions <- function(model, ...){
   UseMethod("test_predictions")
}


test_predictions.default <- function(model, df = model$model, threshold = 0.000000000001, include_intervals = FALSE, max.rows = NULL){
  
  offset <- model$call$offset
  ismodels <- paste0(colnames(model$model), collapse = " ") == paste0(colnames(df), collapse = " ")
  
  if(!is.null(offset) && ismodels){
    index <- which(colnames(df) == "(offset)")
    colnames(df) <- replace(colnames(df), index, as.character(offset))
  }
  
  
  interval <- ifelse(include_intervals == TRUE, "prediction", "none")
  
  if(is.numeric(max.rows)) df <- head(df, max.rows)
  
  base <- predict(model, df, interval = interval)
  
  if(include_intervals == FALSE){
    base <- data.frame(fit = base, row.names =  NULL)
  } else {
    base <- as.data.frame(base)
  }
  
  te <- df %>%
    predict_to_column(model, 
                      add_interval = include_intervals,
                      vars = c("fit_te", "upr_te", "lwr_te")) %>%
    select(contains("_te"))
  
  raw_results <- bind_cols(base, te) %>%
    mutate(fit_diff = abs(fit - fit_te),
           fit_threshold = fit_diff > threshold)
  
  if(include_intervals == TRUE){
    raw_results <- raw_results %>%
      mutate(lwr_diff = abs(lwr - lwr_te),
             upr_diff = abs(upr - upr_te),
             lwr_threshold = lwr_diff > threshold,
             upr_threshold = upr_diff > threshold
             )
  }
  
  raw_results <-raw_results %>%
    rowid_to_column()
  
  threshold_df <- raw_results %>%
    select(contains("_threshold")) %>%
    summarise_all(sum)
  
  alert <- any(threshold_df > 0)
  
  message <- paste0(
    "tidypredict test results\n",
    "Difference threshold: ", threshold,
    "\n"
  )
  
  if(alert){
    
    difference <- raw_results %>%
      select(contains("_diff")) %>%
      summarise_all(max)
    
    message <- paste0(
      message,
      "\nFitted records above the threshold: ", threshold_df$fit_threshold,
      if(!is.null(threshold_df$lwr_threshold)) "\nLower interval records above the threshold: ", threshold_df$lwr_threshold,
      if(!is.null(threshold_df$upr_threshold)) "\nUpper interval records above the threshold: ", threshold_df$upr_threshold,
      "\n\nFit max  difference:", difference$upr_diff,
      "\nLower max difference:", difference$lwr_diff,
      "\nUpper max difference:", difference$fit_diff
    )
  } else {
    message <- paste0(
      message,
      "\n All results are within the difference threshold"
    )
  }
  
  results <- list()
  
  results$model_call <- model$call
  results$raw_results <- raw_results
  results$message <- message
  results$alert <- alert
  
  structure(results, class = c("test_predictions", "list"))
}  

setOldClass(c("test_predictions", "list"))

print.test_predictions <- function(x, ...) {
  cat(x$message)
}


#m3 <- glm(mpg ~ wt + cyl, data = mtcars)
# m3 <- lm(mpg ~ wt + cyl, data = mtcars)
#  
# x <- test_predictions(m3,include_intervals = TRUE, max.rows = 5, threshold =  0.000000000000000000000001)
#  
# x
# 



