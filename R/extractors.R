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
#' @section Which models implement which generic:
#' `.` means the generic is not implemented for that class and will error.
#'
#' ```
#'                  trees  class_trees  class_exprs  n_trees  combine
#' blackboost         x         .            .          x        x
#' C5.0               .         .            .          .        x
#' catboost.Model     x         .            .          x        x
#' cforest            x         .            .          x        x
#' earth              .         .            x          .        .
#' lgb.Booster        x         .            .          x        x
#' multnet            .         .            x          .        .
#' ObliqueForest      x         .            .          x        x
#' party              .         .            x          .        .
#' randomForest       x         x            .          x        x
#' ranger             x         x            .          x        x
#' rpart              .         .            x          .        .
#' xgb.Booster        x         .            .          x        x
#' ```
#'
#' `C50::C5.0()` is the one row with a `tidypredict_combine_trees()` method and
#' no `tidypredict_trees()`. That method exists only to refuse, with an
#' explanation, rather than to let the caller reach the `.default` error and
#' guess why.
#'
#' @section Implementing these for a new model class:
#' The table above shows the grouping to follow. `tidypredict_trees()`,
#' `tidypredict_n_trees()` and `tidypredict_combine_trees()` are a set:
#' implement all three or none. Per-tree expressions are not usable without a
#' count to size them and a rule to recombine them, and shipping the first
#' without the third invites a caller to sum the trees, which is wrong for
#' every backend that carries an offset, a scale or a link.
#'
#' A useful check on a new method is that
#' `tidypredict_combine_trees(x, tidypredict_trees(x))` computes the same
#' values as `tidypredict_fit(x)`. That identity is what the tests for the
#' existing methods assert, and it catches a combination rule that was assumed
#' rather than read out of the model.
#'
#' If a model's trees genuinely cannot be recombined arithmetically, give it a
#' `tidypredict_combine_trees()` method that refuses and no
#' `tidypredict_trees()` method, as `C50::C5.0()` does. Splitting trees apart
#' that cannot be put back together only enables a wrong answer.
#'
#' @seealso [tidypredict_combine_trees()] for turning per-tree expressions back
#'   into a prediction, and [tidypredict_metadata] for what the resulting
#'   values mean.
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
