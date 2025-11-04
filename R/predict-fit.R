#' Returns a Tidy Eval formula to calculate fitted values
#'
#' It parses a model or uses an already parsed model to return a
#' Tidy Eval formula that can then be used inside a dplyr command.
#'
#' @param model An R model or a list with a parsed model.
#'
#' @examples
#'
#' model <- lm(mpg ~ wt + cyl * disp, offset = am, data = mtcars)
#' tidypredict_fit(model)
#' @export
tidypredict_fit <- function(model) {
  UseMethod("tidypredict_fit")
}

#' @export
tidypredict_fit.pm_regression <- function(model) {
  build_fit_formula(model)
}

#' @export
tidypredict_fit.pm_tree <- function(model) {
  if (model$general$model == "cubist") {
    return(tidypredict_fit_cubist(model))
  }
  if (model$general$model == "randomForest") {
    return(tidypredict_fit_randomForest(model))
  }

  res <- generate_case_when_trees(model)

  reduce_addition(res)
}

#' @export
tidypredict_fit.pm_xgb <- function(model) {
  build_fit_formula_xgb(model)
}
