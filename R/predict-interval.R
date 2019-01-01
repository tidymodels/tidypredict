#' Returns a Tidy Eval formula to calculate prediction interval
#'
#' It parses a model or uses an already parsed model to return a
#' Tidy Eval formula that can then be used inside a dplyr command.
#'
#' The result still has to be added to the fit to obtain the upper bound, and
#' subtracted from fit to obtain the lower bound.
#'
#' @param model An R model or a list with a parsed model
#' @param interval The prediction interval, defaults to 0.95
#'
#' @examples 
#' 
#' model <- lm(mpg ~ wt + cyl * disp, offset = am, data = mtcars)
#' tidypredict_interval(model)
#' 
#' @export
tidypredict_interval <- function(model, interval = 0.95) {
  UseMethod("tidypredict_interval")
}

#' @export
`tidypredict_interval.data.frame` <- function(model, interval = 0.95) {
  stop("data.frame based parsed models are no longer supported")
}
