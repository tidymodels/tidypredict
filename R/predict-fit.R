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

# Most models have nothing to do beyond parsing, because the parsed model's own
# `pm_*` class already selects the right builder. Dispatching on the parsed
# class rather than the fitted one keeps that in one place.
#' @export
tidypredict_fit.default <- function(model) {
  if (inherits(model, "parsed_model")) {
    # Parsing again would recurse forever, so a parsed model arriving here means
    # its type has no builder registered.
    cli::cli_abort(
      "Parsed models of type {.val {model$general$type}} are not supported.",
      .internal = TRUE
    )
  }

  has_parser <- any(map_lgl(
    class(model),
    ~ !is.null(utils::getS3method("parse_model", .x, optional = TRUE))
  ))

  if (!has_parser) {
    cli::cli_abort(
      "Models of class {.cls {class(model)[[1]]}} are not supported."
    )
  }

  tidypredict_fit(parse_model(model))
}

#' @export
tidypredict_fit.pm_tree <- function(model) {
  version <- model$general$version %||% 1

  # Version 3: nested case_when format, one builder per model
  if (version >= 3) {
    return(build_tree_formula(as_pm_tree_model(model)))
  }

  # Version 1/2: flat case_when format (backwards compatibility with saved models)
  if (model$general$model == "cubist") {
    return(tidypredict_fit_cubist(model))
  }
  if (model$general$model == "randomForest") {
    return(tidypredict_fit_randomForest(model))
  }
  if (model$general$model == "ranger") {
    return(tidypredict_fit_ranger(model))
  }
}

# Tree parsed models all share the `pm_tree` class, so the model they came from
# only shows up as a string. Turn that string into a class so each model's
# builder can be a method living beside the rest of its code.
#
# The class is added here rather than in `as_parsed_model()` so that parsed
# models serialized before this existed still dispatch correctly.
as_pm_tree_model <- function(model) {
  class(model) <- c(paste0("pm_tree_", model$general$model), class(model))
  model
}

build_tree_formula <- function(model) {
  UseMethod("build_tree_formula")
}

build_tree_formula.default <- function(model) {
  cli::cli_abort(
    "No builder for tree model {.val {model$general$model}}.",
    .internal = TRUE
  )
}

# Forests average their trees.
build_tree_formula_forest <- function(model) {
  expr_mean(map(model$tree_info_list, generate_nested_case_when_tree))
}

# A single tree is just its own nested `case_when()`.
build_tree_formula_single <- function(model) {
  generate_nested_case_when_tree(model$tree_info)
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
