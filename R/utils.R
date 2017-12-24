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
    select(fit_te, lwr_te, upr_te)
  
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
  
  #raw_results
  structure(raw_results, class = c("test_predictions", "data.frame"))
}  

setOldClass(c("test_predictions", "data.frame"))

print.test_predictions <- function(x, ...) {
  
  fit <- filter(x, fit_threshold)
  fit_records <- nrow(fit)
  cat("\nFitted records over the threshold: ", fit_records)
  if(fit_records > 0){
    cat(fit %>%
          mutate(message = paste0("\n  Row:", rowid, " |  Difference: ", fit_diff)) %>%
          pull() )
    }


  lwr <- filter(x, lwr_threshold)
  lwr_records <- nrow(lwr)
  cat("\nLower interval records over the threshold: ", lwr_records)
  if(lwr_records > 0){
    cat(lwr %>%
          mutate(message = paste0("\n  Row:", rowid, " |  Difference: ", lwr_diff)) %>%
          pull() )
  } 

  upr <- filter(x, upr_threshold)
  upr_records <- nrow(upr)
  cat("\nUpper interval records over the threshold: ", upr_records)
  if(upr_records > 0){
    cat(upr %>%
          mutate(message = paste0("\n  Row:", rowid, " |  Difference: ", upr_diff)) %>%
          pull() )
  }
  
    
}


# m3 <- lm(mpg ~ wt + cyl, data = mtcars)
# 
# x <- test_predictions(m3,include_intervals = TRUE, max.rows = 5, threshold =  0.000000000000000000000001)
# 
# x





