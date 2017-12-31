#' Returns a Tidy Eval formula to calculate prediction interval
#'
#' It uses parse_model to create a tabular version of the data needed to create
#' a Tidy Eval formula that can then be used inside a dplyr command.  It uses
#' S3 methods to automatically determine the class of the model and run the
#' corresponding function that is able to create the Tidy Eval formula.  It
#' currently supports lm() models.
#'
#' The result still has to be added to the fit to obtain the upper bound, and
#' substracted from fit to obtain the lower bound.
#'
#' @param model An R model or a tibble with a parsed model
#' @param interval The prediction interval, defaults to 0.95
#'
#' @examples
#'
#' df <- data.frame(x = c(1, 2, 5, 6 ,6), y = c(2, 3, 6, 5, 4))
#' model <- lm(x ~ y, df)
#' tidypredict_interval(model)
#'
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
    filter(labels == "model") %>%
    pull(vals)

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
