#' Converts an R model object into a table
#'
#' It parses a fitted R model's structure and extracts the components
#' needed to create a dplyr formula for prediction.  The function also
#' creates a data frame using an specific format so that other
#' functions in the future can also pass parsed tables to a given
#' formula creating function.
#'
#' @param model An R model object. It currently supports lm(),
#' glm() and randomForest() models.
#'
#' @examples
#' library(dplyr)
#' df <- mutate(mtcars, cyl = paste0("cyl", cyl))
#' model <- lm(mpg ~ wt + cyl * disp, offset = am, data = df)
#' parse_model(model)
#'
#' @export
parse_model <- function(model) {
  UseMethod("parse_model")
}
#' @import dplyr
#' @importFrom tibble tibble
add_variable <- function(df, labels, vals) {
  df %>%
    bind_rows(tibble(
      labels = !!labels,
      vals = as.character(!!vals),
      type = "variable"
    ))
}

#' @export
`tidypredict_fit.data.frame` <- function(model) {
  model <- model %>%
    mutate_if(is.factor, as.character) %>%
    as.tibble()
  
  model_type <- model %>%
    filter(.data$labels == "model") %>%
    pull(.data$vals)
  
  assigned <- 0
  
  if (model_type == "lm") {
    assigned <- 1
    fit <- te_fit_lm(model)
  }
  
  if (model_type == "glm") {
    assigned <- 1
    fit <- te_fit_glm(model)
  }
  
  if (model_type == "randomForest") {
    assigned <- 1
    fit <- te_randomforest_fit(model)
  }
  
  if (model_type == "ranger") {
    assigned <- 1
    fit <- te_ranger_fit(model)
  }
  
  if (model_type == "earth") {
    assigned <- 1
    fit <- te_earth_fit(model)
  }
  
  if (assigned == 0) {
    stop("Model not recognized")
  }
  
  fit
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