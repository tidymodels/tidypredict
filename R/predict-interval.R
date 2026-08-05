#' Returns a Tidy Eval formula to calculate prediction interval.
#'
#' It parses a model or uses an already parsed model to return a
#' Tidy Eval formula that can then be used inside a dplyr command.
#'
#' The result still has to be added to and subtracted from the fit to obtain the upper and
#' lower bound respectively.
#'
#' @param model An R model or a list with a parsed model
#' @param interval The prediction interval, defaults to 0.95
#'
#' @examples
#'
#' model <- lm(mpg ~ wt + cyl * disp, offset = am, data = mtcars)
#' tidypredict_interval(model)
#' @export
tidypredict_interval <- function(model, interval = 0.95) {
  UseMethod("tidypredict_interval")
}

# Prediction intervals only exist for the linear models, so every other class
# lands here rather than on R's "no applicable method" error.
#' @export
tidypredict_interval.default <- function(model, interval = 0.95) {
  cli::cli_abort(c(
    "Prediction intervals are not supported for {.cls {class(model)[[1]]}} models.",
    i = "Only {.cls lm} and {.cls glm} models have prediction intervals."
  ))
}

#' @export
`tidypredict_interval.data.frame` <- function(model, interval = 0.95) {
  cli::cli_abort("data.frame based parsed models are no longer supported.")
}

#' @export
tidypredict_interval.list <- function(model, interval = 0.95) {
  mt <- model$general$model
  fit <- NULL
  if (mt == "lm") {
    fit <- te_interval_lm(model, interval)
  }
  if (mt == "glm") {
    fit <- te_interval_glm(model, interval)
  }
  if (is.null(fit)) {
    cli::cli_abort("Model type not supported.")
  }
  fit
}
