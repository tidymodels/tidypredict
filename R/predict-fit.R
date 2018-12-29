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
#' library(dplyr)
#' df <- mutate(mtcars, cyl = paste0("cyl", cyl))
#' model <- lm(mpg ~ wt + cyl * disp, offset = am, data = df)
#' tidypredict_fit(model)
#'
#' @export
tidypredict_fit <- function(model) {
  UseMethod("tidypredict_fit")
}

#' @export
tidypredict_fit.randomForest <- function(model) {
  parsedmodel <- parse_model(model)
  te_randomforest_fit(parsedmodel)
}

#' @export
tidypredict_fit.ranger <- function(model) {
  parsedmodel <- parse_model(model)
  te_ranger_fit(parsedmodel)
}

