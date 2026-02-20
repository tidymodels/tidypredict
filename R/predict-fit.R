#' Returns a Tidy Eval formula to calculate fitted values
#'
#' It parses a model or uses an already parsed model to return a
#' Tidy Eval formula that can then be used inside a dplyr command.
#'
#' @param model An R model or a list with a parsed model.
#' @param ... Additional arguments passed to methods.
#'
#' @examples
#'
#' model <- lm(mpg ~ wt + cyl * disp, offset = am, data = mtcars)
#' tidypredict_fit(model)
#' @export
tidypredict_fit <- function(model, ...) {
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
  if (model$general$model == "ranger") {
    return(tidypredict_fit_ranger(model))
  }

  res <- generate_case_when_trees(model)

  reduce_addition(res)
}

#' @export
tidypredict_fit.pm_xgb <- function(model) {
  build_fit_formula_xgb(model)
}

#' @export
tidypredict_fit.pm_lgb <- function(model) {
  build_fit_formula_lgb(model)
}

#' @export
tidypredict_fit.pm_catboost <- function(model) {
  build_fit_formula_catboost(model)
}
