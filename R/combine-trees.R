#' Combine per-tree expressions into a model's prediction
#'
#' @description
#' [tidypredict_trees()] returns one expression per tree. Turning those back
#' into the model's prediction is not simply summing or averaging them, and the
#' rule differs by backend: `mboost::blackboost()` needs an offset and a
#' shrinkage factor, CatBoost needs a scale and a bias, `aorsf` needs a guard
#' that returns `NA` for an incomplete row, and boosters then apply their
#' objective's inverse link on top.
#'
#' This generic holds that rule, so a caller that has split the trees apart
#' can put them back together without knowing which backend it is holding.
#'
#' @param x A fitted model object.
#' @param trees A list of expressions, one per tree, in the order
#'   [tidypredict_trees()] returns them. Typically either that return value
#'   itself, or symbols naming the columns the individual trees were written
#'   to.
#' @param ... Additional arguments passed to methods.
#'
#' @returns A single language object.
#'
#' @details
#' The point of separating `trees` from this function is that a caller can
#' compute each tree into its own column, for a database to evaluate in
#' parallel, and then pass symbols referring to those columns rather than the
#' expressions themselves. The combination is the same either way.
#'
#' A caller may also pass fewer elements than the model has trees, where each
#' one is a subtotal of several trees added together. This is what a caller
#' does to keep any single generated expression short enough for a database
#' parser. Every method supports it: those that average take the divisor from
#' the model rather than from `length(trees)`, and the rest sum. The one thing
#' a caller must not do is reorder or drop trees, since a method is free to
#' treat position as meaningful.
#'
#' Every ensemble satisfies
#' `tidypredict_combine_trees(x, tidypredict_trees(x))` computing the same
#' values as `tidypredict_fit(x)`, and that identity is what the tests for
#' these methods assert.
#'
#' Not every ensemble has a method. `C50::C5.0()` boosting combines its trees
#' by a confidence-weighted vote that yields a class label, so there is no
#' arithmetic to apply to per-tree numbers and no method is provided.
#'
#' @seealso [tidypredict_extractors] for the generics that produce `trees`,
#'   including a table of which model classes implement them, and
#'   [tidypredict_metadata] for what the combined value means.
#'
#' @examplesIf rlang::is_installed("randomForest")
#' model <- randomForest::randomForest(mpg ~ ., data = mtcars, ntree = 3)
#'
#' trees <- tidypredict_trees(model)
#' tidypredict_combine_trees(model, trees)
#'
#' # Or referring to columns the trees were written to first.
#' tidypredict_combine_trees(model, rlang::syms(c("t1", "t2", "t3")))
#'
#' @export
tidypredict_combine_trees <- function(x, trees, ...) {
  UseMethod("tidypredict_combine_trees")
}

#' @export
tidypredict_combine_trees.default <- function(x, trees, ...) {
  cli::cli_abort(
    "{.fn tidypredict_combine_trees} is not available for models of class
     {.cls {class(x)[[1]]}}.",
    class = "tidypredict_no_combiner"
  )
}

check_trees_arg <- function(trees, call = rlang::caller_env()) {
  if (!is.list(trees) || length(trees) == 0) {
    cli::cli_abort(
      "{.arg trees} must be a non-empty list of expressions, not
       {.obj_type_friendly {trees}}.",
      call = call
    )
  }

  invisible(trees)
}
