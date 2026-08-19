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
#' @returns A Tidy Eval formula that calculates the half width of the
#'   prediction interval. It must be added to and subtracted from the fit to
#'   obtain the upper and lower bounds.
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
  abort_interval_unsupported(model)
}

# A parsed model always inherits `"list"`, so `tidypredict_interval.list()`
# lands here too rather than repeating the wording.
abort_interval_unsupported <- function(model, call = rlang::caller_env()) {
  what <- if (is.list(model) && is.list(model$general)) {
    mt <- model$general$model %||% model$general$type %||% "unknown"
    "{.val {mt}} parsed models"
  } else {
    "{.cls {class(model)[[1]]}} models"
  }

  cli::cli_abort(
    c(
      "Prediction intervals are not supported for {cli::format_inline(what)}.",
      i = "Only {.cls lm} and {.cls glm} models have prediction intervals."
    ),
    call = call
  )
}

#' @export
`tidypredict_interval.data.frame` <- function(model, interval = 0.95) {
  cli::cli_abort("data.frame based parsed models are no longer supported.")
}

#' @export
tidypredict_interval.list <- function(model, interval = 0.95) {
  check_parsed_model(model)
  check_interval(interval)

  # `mt` is `NULL` for a list that is not a parsed model at all, and `switch()`
  # needs a string, so the comparison used to fail with "argument is of length
  # zero" instead of saying what was wrong.
  mt <- model$general$model %||% ""

  switch(
    mt,
    lm = te_interval_lm(model, interval),
    glm = te_interval_glm(model, interval),
    abort_interval_unsupported(model)
  )
}
