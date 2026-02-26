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
  version <- model$general$version %||% 1

  # Version 3: nested case_when format
  if (version >= 3) {
    model_type <- model$general$model
    if (model_type == "cubist") {
      return(tidypredict_fit_cubist(model))
    }
    if (model_type %in% c("rpart", "party")) {
      return(generate_nested_case_when_tree(model$tree_info))
    }
    if (model_type %in% c("ranger", "randomForest")) {
      # For forests, average all trees
      tree_exprs <- map(model$tree_info_list, generate_nested_case_when_tree)
      res <- reduce_addition(tree_exprs)
      return(expr_division(res, length(tree_exprs)))
    }
  }

  # Version 1/2: flat case_when format (backwards compatibility)
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
  version <- model$general$version %||% 1

  if (version >= 3) {
    return(build_fit_formula_xgb_from_parsed(model))
  }

  # Version 1/2: flat case_when (backwards compatibility)
  build_fit_formula_xgb(model)
}

#' @export
tidypredict_fit.pm_lgb <- function(model) {
  build_fit_formula_lgb_from_parsed(model)
}

#' @export
tidypredict_fit.pm_catboost <- function(model) {
  build_fit_formula_catboost_nested(model)
}
