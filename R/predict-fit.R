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

  # Version 3: nested case_when format
  if (version >= 3) {
    model_type <- model$general$model
    if (model_type == "cubist") {
      return(tidypredict_fit_cubist(model))
    }
    if (model_type %in% c("rpart", "party", "C5.0")) {
      if (model_type == "C5.0" && !is.null(model$rules_info)) {
        return(c50_rules_case_when(model$rules_info))
      }
      if (model_type == "C5.0" && !is.null(model$tree_info_list)) {
        return(c50_boosted_case_when(model$tree_info_list, model$classes))
      }
      return(generate_nested_case_when_tree(model$tree_info))
    }
    if (model_type == "bagger") {
      return(bagger_build_formula(model))
    }
    if (model_type == "blackboost") {
      return(mboost_build_formula(
        model$tree_info_list,
        model$general$nu,
        model$general$offset
      ))
    }
    if (model_type %in% c("ranger", "randomForest", "cforest", "aorsf")) {
      # For forests, average all trees
      return(expr_mean(map(
        model$tree_info_list,
        generate_nested_case_when_tree
      )))
    }
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
