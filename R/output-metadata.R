#' Describe what a model's fitted expressions compute
#'
#' @description
#' `tidypredict_fit()` returns expressions, but not what those expressions
#' mean. A single expression could be a numeric prediction, a probability, or
#' an uncalibrated decision value, and the three call for different handling
#' downstream. These generics answer that question, so that a package
#' generating code from the result does not have to keep its own list of which
#' backend produces which shape.
#'
#' The metadata is asked of the *model*, not of the fitted expressions,
#' deliberately. Attributes on the result do not survive the subsetting,
#' `lapply()` and `unlist()` that callers apply to a multiclass result, which
#' is the case that most needs describing.
#'
#' @param x A fitted model object.
#' @param ... Additional arguments passed to methods.
#'
#' @returns
#' `tidypredict_output_type()` returns a single string, one of:
#'
#' \describe{
#'   \item{`"numeric"`}{A numeric prediction. `tidypredict_fit()` returns one
#'     expression, or a named list of them for a multivariate outcome or for a
#'     quantile regression with several `tau`.}
#'   \item{`"prob"`}{A probability. Either one expression giving the
#'     probability of the second outcome level, for a binary model, or a list
#'     with one expression per level.}
#'   \item{`"decision"`}{An uncalibrated decision value whose *sign* selects
#'     the class. Not a probability, and not comparable to one: the cut is at
#'     0, not 0.5.}
#'   \item{`"class"`}{A hard class prediction, with no probability available.
#'     Usually the class label as a string, but `xgboost`'s `binary:hinge`
#'     objective gives a 0/1 indicator instead. What makes it `"class"` rather
#'     than `"numeric"` is that only the class values can occur, so using it as
#'     a numeric prediction is a mistake even when its type is numeric.}
#' }
#'
#' `tidypredict_outcome_levels()` returns a character vector of outcome levels
#' in model order, or `NULL`.
#'
#' `NULL` means two different things, and `tidypredict_output_type()`
#' distinguishes them. For a `"numeric"` model it means there are no levels.
#' For a `"prob"` or `"class"` model it means the fitted model **did not retain
#' the outcome levels**, so any names on the result are positional
#' placeholders and the caller has to supply the real levels from elsewhere.
#' LightGBM and CatBoost multiclass models are in this position: they store
#' integer labels and their expressions come back named `class_0`, `class_1`
#' and so on.
#'
#' `tidypredict_normalized()` returns `TRUE` if the per-level values already
#' sum to one across levels, `FALSE` if the caller has to normalize them, and
#' `NA` when there are no per-level values to sum, which includes every
#' single-expression model.
#'
#' At present no backend returns `FALSE`: every multiclass probability list
#' goes through one shared softmax, so the values are always normalized
#' already. The generic exists so that a caller can rely on that rather than
#' having to know it, and so a future backend that does not normalize can say
#' so instead of silently breaking the assumption.
#'
#' @details
#' None of this is recoverable from the shape of the result, which is the whole
#' reason for recording it. Two concrete cases:
#'
#' A binary `"prob"` model and a `"decision"` model both return exactly one
#' expression. `LiblineaR` produces either, depending only on its `type`
#' argument. Treating a decision value as a probability and cutting it at 0.5
#' gives silently wrong classes for every row whose value falls between 0 and
#' 0.5.
#'
#' A multiclass `"prob"` model and a `quantreg::rq()` fit with several `tau`
#' both return a named list of expressions of the same length and structure.
#' In the first the values sum to one across the list; in the second they are
#' unrelated numeric predictions.
#'
#' @seealso [tidypredict_extractors] for the generics that expose a model's
#'   per-tree and per-level expressions, and [tidypredict_combine_trees()] for
#'   recombining them.
#'
#' @examplesIf rlang::is_installed("MASS")
#' model <- lm(mpg ~ wt, data = mtcars)
#' tidypredict_output_type(model)
#' tidypredict_outcome_levels(model)
#' tidypredict_normalized(model)
#'
#' @name tidypredict_metadata
NULL

#' @rdname tidypredict_metadata
#' @export
tidypredict_output_type <- function(x, ...) {
  UseMethod("tidypredict_output_type")
}

# Most models describe themselves adequately once parsed, and the parsed
# classes are far fewer than the model classes, so the default routes through
# `parse_model()` the same way `tidypredict_fit.default()` does. A model whose
# parsed form is not enough, such as LiblineaR, gets its own method.
#' @export
tidypredict_output_type.default <- function(x, ...) {
  metadata_via_parsed(
    x,
    tidypredict_output_type,
    "tidypredict_output_type",
    ...
  )
}

metadata_via_parsed <- function(
  x,
  generic,
  generic_name,
  ...,
  call = rlang::caller_env()
) {
  # A parsed model reaching the default means its own type has no method;
  # parsing again would recurse forever.
  if (inherits(x, "parsed_model")) {
    abort_no_metadata(x, generic_name, call = call)
  }

  has_parser <- any(map_lgl(
    class(x),
    ~ !is.null(utils::getS3method("parse_model", .x, optional = TRUE))
  ))
  if (!has_parser) {
    abort_no_metadata(x, generic_name, call = call)
  }

  generic(parse_model(x), ...)
}

#' @rdname tidypredict_metadata
#' @export
tidypredict_outcome_levels <- function(x, ...) {
  UseMethod("tidypredict_outcome_levels")
}

#' @export
tidypredict_outcome_levels.default <- function(x, ...) {
  metadata_via_parsed(
    x,
    tidypredict_outcome_levels,
    "tidypredict_outcome_levels",
    ...
  )
}

#' @rdname tidypredict_metadata
#' @export
tidypredict_normalized <- function(x, ...) {
  UseMethod("tidypredict_normalized")
}

#' @export
tidypredict_normalized.default <- function(x, ...) {
  metadata_via_parsed(x, tidypredict_normalized, "tidypredict_normalized", ...)
}

# Parsed model methods ----------------------------

# `build_fit_formula()` applies the inverse link when `is_glm` is 1, so a
# binomial model's expression already is a probability. Every other family
# returns a conditional mean on the response scale, which is a number rather
# than a probability: a Poisson count of 3.2 is not 320% of anything.
#' @export
tidypredict_output_type.pm_regression <- function(x, ...) {
  rlang::check_dots_empty()

  if (
    identical(x$general$is_glm, 1) && identical(x$general$family, "binomial")
  ) {
    return("prob")
  }

  "numeric"
}

#' @export
tidypredict_normalized.pm_regression <- function(x, ...) {
  rlang::check_dots_empty()

  # One expression, so there is no set of per-level values to sum.
  NA
}

# The parsed form of a regression keeps coefficients, not the outcome. The
# binary probability backends that do record their response levels (glm,
# `lognet`, `ksvm`) answer on the fitted class instead.
#' @export
tidypredict_outcome_levels.pm_regression <- function(x, ...) {
  rlang::check_dots_empty()
  NULL
}

# Every one of these produces one expression per outcome level.
#' @export
tidypredict_output_type.pm_multiclass_regression <- function(x, ...) {
  rlang::check_dots_empty()
  "prob"
}

#' @export
tidypredict_output_type.pm_naive_bayes <- function(x, ...) {
  rlang::check_dots_empty()
  "prob"
}

#' @export
tidypredict_output_type.pm_nullmodel_classification <- function(x, ...) {
  rlang::check_dots_empty()
  "prob"
}

# All three keep the outcome levels in `classes` and name the expressions after
# them, and all three finish with `expr_softmax()` or an explicit set of
# probabilities, so the per-level values already sum to one.
#' @export
tidypredict_outcome_levels.pm_multiclass_regression <- function(x, ...) {
  rlang::check_dots_empty()
  parsed_model_classes(x)
}

#' @export
tidypredict_normalized.pm_multiclass_regression <- function(x, ...) {
  rlang::check_dots_empty()
  TRUE
}

#' @export
tidypredict_outcome_levels.pm_naive_bayes <- function(x, ...) {
  rlang::check_dots_empty()
  parsed_model_classes(x)
}

#' @export
tidypredict_normalized.pm_naive_bayes <- function(x, ...) {
  rlang::check_dots_empty()
  TRUE
}

#' @export
tidypredict_outcome_levels.pm_nullmodel_classification <- function(x, ...) {
  rlang::check_dots_empty()
  parsed_model_classes(x)
}

#' @export
tidypredict_normalized.pm_nullmodel_classification <- function(x, ...) {
  rlang::check_dots_empty()
  TRUE
}

# `classes` is a bare character vector for some backends and a list of strings
# for others, which is also how `build_fit_formula_*()` reads it.
parsed_model_classes <- function(x) {
  classes <- as.character(unlist(x$classes))
  if (length(classes) == 0) {
    return(NULL)
  }
  classes
}

# `pm_tree` spans ten backends with three different modes between them, and
# `general$model` is what the rest of the tree code switches on, so switch on it
# here too. `ranger`, `randomForest`, `cforest`, `aorsf`, `blackboost` and
# `cubist` all abort at parse time for a classification fit, so a parsed model
# under those names is necessarily a regression. `bagger` records its levels in
# `general$classes` when, and only when, it is a classification.
#
# `rpart` and `party` are the exception: their parsed form keeps the tree and
# nothing that says which mode it came from, so they answer on the fitted class
# and a parsed model on its own cannot be described.
tree_regression_models <- c(
  "ranger",
  "randomForest",
  "cforest",
  "aorsf",
  "blackboost",
  "cubist"
)

#' @export
tidypredict_output_type.pm_tree <- function(x, ...) {
  rlang::check_dots_empty()

  model <- x$general$model
  if (model %in% tree_regression_models) {
    return("numeric")
  }
  if (identical(model, "C5.0")) {
    return("class")
  }
  if (identical(model, "bagger")) {
    if (is.null(parsed_model_classes(x$general))) {
      return("numeric")
    }
    return("class")
  }

  abort_no_metadata(x, "tidypredict_output_type")
}

#' @export
tidypredict_outcome_levels.pm_tree <- function(x, ...) {
  rlang::check_dots_empty()

  model <- x$general$model
  # `parse_model.C5.0()` keeps the tree but not `model$levels`, so the parsed
  # form genuinely has no levels to report. `tidypredict_outcome_levels.C5.0()`
  # answers from the fitted object.
  if (model %in% c(tree_regression_models, "C5.0")) {
    return(NULL)
  }
  if (identical(model, "bagger")) {
    return(parsed_model_classes(x$general))
  }

  abort_no_metadata(x, "tidypredict_outcome_levels")
}

#' @export
tidypredict_normalized.pm_tree <- function(x, ...) {
  rlang::check_dots_empty()

  model <- x$general$model
  if (model %in% c(tree_regression_models, "C5.0", "bagger")) {
    # Every tree backend produces a single expression, a number or a class
    # label, so there are never per-level values to sum.
    return(NA)
  }

  abort_no_metadata(x, "tidypredict_normalized")
}

# Aborting rather than guessing "numeric". A wrong answer here is not a visible
# failure downstream, it is mis-named or mis-thresholded predictions, so a
# backend with no method must say so rather than be assumed benign.
abort_no_metadata <- function(x, generic, call = rlang::caller_env()) {
  cli::cli_abort(
    "{.fn {generic}} is not available for models of class
     {.cls {class(x)[[1]]}}.",
    class = "tidypredict_no_metadata",
    call = call
  )
}
