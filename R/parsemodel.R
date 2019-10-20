#' Converts an R model object into a table
#'
#' It parses a fitted R model's structure and extracts the components
#' needed to create a dplyr formula for prediction.  The function also
#' creates a data frame using an specific format so that other
#' functions in the future can also pass parsed tables to a given
#' formula creating function.
#'
#' @param model An R model object.
#'
#' @examples
#' library(dplyr)
#' df <- mutate(mtcars, cyl = paste0("cyl", cyl))
#' model <- lm(mpg ~ wt + cyl * disp, offset = am, data = df)
#' parse_model(model)
#' @export
parse_model <- function(model) {
  UseMethod("parse_model")
}
