#' Returns a Tidy Eval formula to calculate prediction interval
#'
#' It parses a model or uses an already parsed model to return a
#' Tidy Eval formula that can then be used inside a dplyr command.
#'
#' The result still has to be added to the fit to obtain the upper bound, and
#' subtracted from fit to obtain the lower bound.
#'
#' @param model An R model or a tibble with a parsed model. It currently supports
#' lm() models only.
#' @param interval The prediction interval, defaults to 0.95
#'
#' @examples
#'
#'library(dplyr)
#'df <- mutate(mtcars, cyl = paste0("cyl", cyl))
#'model <- lm(mpg ~ wt + cyl * disp, offset = am, data = df)
#'tidypredict_interval(model)
#'
#' @export
tidypredict_interval <- function(model, interval = 0.95) {
  UseMethod("tidypredict_interval")
}

#' @export
tidypredict_interval.lm <- function(model, interval = 0.95) {
  parsedmodel <- parse_model(model)
  te_interval_lm(parsedmodel, interval)
}

#' @export
tidypredict_interval.glm <- function(model, interval = 0.95) {
  parsedmodel <- parse_model(model)
  te_interval_glm(parsedmodel, interval)
}

#' @export
`tidypredict_interval.data.frame` <- function(model, interval = 0.95) {
  model <- model %>%
    mutate_if(is.factor, as.character) %>%
    as.tibble()

  model_type <- model %>%
    filter(.data$labels == "model") %>%
    pull(.data$vals)

  assigned <- 0

  if (model_type == "lm") {
    assigned <- 1
    te_interval_lm(model)
  }

  if (model_type == "glm") {
    assigned <- 1
    te_interval_glm(model)
  }

  if (assigned == 0) {
    stop("Model not recognized")
  }
}
