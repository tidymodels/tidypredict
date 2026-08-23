#' Extract model internals as expressions
#'
#' @description
#' These generics expose the pieces `tidypredict_fit()` is assembled from,
#' rather than the finished formula. They exist so that packages generating
#' their own code from a fitted model, such as orbital, can reuse
#' tidypredict's parsing instead of reimplementing it per model class.
#'
#' Each generic has a single fixed return shape, described below. A model class
#' implements whichever generics make sense for it: a random forest has trees
#' and a tree count, a `glmnet` multinomial model has neither.
#'
#' @param x A fitted model object.
#' @param ... Additional arguments passed to methods. `multnet` models accept
#'   `penalty`, which is required when the model was fitted with more than one
#'   value of lambda.
#'
#' @returns
#' `tidypredict_trees()` returns an unnamed list with one element per tree,
#' each a language object.
#'
#' `tidypredict_class_trees()` returns a list named by outcome level, in model
#' order. Each element is itself an unnamed list of per-tree language objects
#' for that level, so the result is `tidypredict_trees()` nested one level
#' deeper. What the leaves hold depends on the model: `randomForest` gives
#' 0/1 votes, `ranger` gives class probabilities.
#'
#' `tidypredict_class_exprs()` returns a list named by outcome level, in model
#' order, with one language object per level. Unlike
#' `tidypredict_class_trees()` there is no per-tree structure and nothing to
#' combine: each expression computes that level's value on its own.
#'
#' `tidypredict_n_trees()` returns a single integer, the number of trees in the
#' ensemble. For multiclass boosters this counts every tree, including the
#' per-class copies, so it is not the same as the number of boosting rounds.
#'
#' Wherever an expression is described above, a **bare numeric value** may
#' appear in its place when the model has nothing to branch on. Callers must
#' handle both. This happens for a single-leaf tree, a stump, and also for a
#' degenerate expression such as a `glmnet` class whose coefficients are all
#' zero. Note that the constant can appear alongside language objects in the
#' same result, so the element type is not uniform within one list.
#'
#' @details
#' Two shapes that look similar are worth keeping apart.
#' `tidypredict_class_trees()` returns many trees per level that a caller has
#' to sum or average, and needs `tidypredict_n_trees()` to do it.
#' `tidypredict_class_exprs()` returns one finished expression per level. Both
#' are named by outcome level so that callers never have to assume the order
#' matches `levels()` of the outcome.
#'
#' @examplesIf rlang::is_installed("randomForest")
#' model <- randomForest::randomForest(mpg ~ ., data = mtcars, ntree = 5)
#'
#' tidypredict_n_trees(model)
#'
#' trees <- tidypredict_trees(model)
#' length(trees)
#' trees[[1]]
#'
#' @name tidypredict_extractors
NULL

#' @rdname tidypredict_extractors
#' @export
tidypredict_trees <- function(x, ...) {
  UseMethod("tidypredict_trees")
}

#' @export
tidypredict_trees.default <- function(x, ...) {
  abort_no_extractor(x, "tidypredict_trees")
}

#' @rdname tidypredict_extractors
#' @export
tidypredict_class_trees <- function(x, ...) {
  UseMethod("tidypredict_class_trees")
}

#' @export
tidypredict_class_trees.default <- function(x, ...) {
  abort_no_extractor(x, "tidypredict_class_trees")
}

#' @rdname tidypredict_extractors
#' @export
tidypredict_class_exprs <- function(x, ...) {
  UseMethod("tidypredict_class_exprs")
}

#' @export
tidypredict_class_exprs.default <- function(x, ...) {
  abort_no_extractor(x, "tidypredict_class_exprs")
}

#' @rdname tidypredict_extractors
#' @export
tidypredict_n_trees <- function(x, ...) {
  UseMethod("tidypredict_n_trees")
}

#' @export
tidypredict_n_trees.default <- function(x, ...) {
  abort_no_extractor(x, "tidypredict_n_trees")
}

# Distinct from abort_model_unsupported(): the model class may well be
# supported by tidypredict_fit() and simply not expose this particular piece.
# A random forest has trees, a glmnet model does not.
abort_no_extractor <- function(x, generic, call = rlang::caller_env()) {
  cli::cli_abort(
    "{.fn {generic}} is not available for models of class
     {.cls {class(x)[[1]]}}.",
    class = "tidypredict_no_extractor",
    call = call
  )
}
