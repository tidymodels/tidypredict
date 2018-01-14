#' Returns a Tidy Eval formula to calculate fitted values
#'
#' It parses a model or uses an already parsed model to return a
#' Tidy Eval formula that can then be used inside a dplyr command.  
#'
#' @param model An R model or a tibble with a parsed model. It currently supports
#' lm(), glm() and randomForest() models.
#'
#' @examples
#'
#'library(dplyr)
#'df <- mutate(mtcars, cyl = paste0("cyl", cyl))
#'model <- lm(mpg ~ wt + cyl * disp, offset = am, data = df)
#'tidypredict_fit(model)
#'
#' @export
tidypredict_fit <- function(model) {
  UseMethod("tidypredict_fit")
}

#' @export
tidypredict_fit.lm <- function(model) {
  parsedmodel <- parse_model(model)
  te_fit_lm(parsedmodel)
}

#' @export
tidypredict_fit.glm <- function(model) {
  parsedmodel <- parse_model(model)
  te_fit_glm(parsedmodel)
}

#' @export
tidypredict_fit.randomForest <- function(model) {
  parsedmodel <- parse_model(model)
  te_randomforest_fit(parsedmodel)
}


#' @export
#' @importFrom tibble as.tibble
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


  if (assigned == 0) {
    stop("Model not recognized")
  }

  fit
}
