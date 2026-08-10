#' Generate nested case_when trees
#'
#' These functions generate nested `case_when()` expressions for decision trees,
#' which are more efficient than flat `case_when()` for both R/dplyr and SQL
#' execution.
#'
#' The following tree:
#'
#'             +-----+
#'        +----|x > 0|----+
#'        |    +-----+    |
#'        v               v
#'    +------+        +--------+
#' +--|y < 20|--+  +--|z <= 10 |--+
#' |  +------+  |  |  +--------+  |
#' v            v  v              v
#' a            b  c              d
#'
#' will be turned into the following nested `case_when()` statement:
#'
#' ```r
#' case_when(
#'   x > 0 ~ case_when(
#'     y < 20 ~ "a",
#'     .default = "b"
#'   ),
#'   .default = case_when(
#'     z <= 10 ~ "c",
#'     .default = "d"
#'   )
#' )
#' ```
#'
#' @details
#' By default a missing predictor takes the `.default` branch of every split it
#' reaches, which is what the model's own traversal does for some backends but
#' not others. Pass `na_propagate = TRUE` to return `NA` instead as soon as a
#' row's path reaches a split on a column it is missing.
#'
#' @keywords internal
#' @noRd
NULL

#' Generate nested case_when for a tree
#'
#' @param tree_info A tree info list from `rpart_tree_info_full()` or similar
#' @param na_propagate Return `NA` for a row once its path reaches a split on
#'   a column it is missing, rather than sending it down the `.default` branch.
#' @keywords internal
generate_nested_case_when_tree <- function(tree_info, na_propagate = FALSE) {
  build_nested_node(0L, tree_info, na_propagate)
}

# Restate one tree's structure with different leaf values, as classification
# models do when they need one tree per class out of a single fitted tree.
tree_info_with_predictions <- function(tree_info, prediction) {
  tree_info$prediction <- prediction
  tree_info
}

#' Build a nested case_when expression for a single node
#'
#' @param node_id The node ID to build (0-indexed)
#' @param tree_info Tree info list with nodeID, leftChild, rightChild,
#'   splitvarName, terminal, prediction, and node_splits
#' @param na_propagate Return `NA` for a row once its path reaches a split on
#'   a column it is missing, rather than sending it down the `.default` branch.
#' @keywords internal
build_nested_node <- function(node_id, tree_info, na_propagate = FALSE) {
  node_idx <- which(tree_info$nodeID == node_id)

  # Leaf node: return prediction
  if (tree_info$terminal[node_idx]) {
    prediction <- tree_info$prediction[node_idx]
    if (is.factor(prediction)) {
      prediction <- as.character(prediction)
    }
    return(prediction)
  }

  # Internal node: build nested case_when
  left_id <- tree_info$leftChild[node_idx]
  right_id <- tree_info$rightChild[node_idx]

  # Get split info
  split_info <- tree_info$node_splits[[node_idx]]

  # Recursively build subtrees
  left_subtree <- build_nested_node(left_id, tree_info, na_propagate)
  right_subtree <- build_nested_node(right_id, tree_info, na_propagate)

  # Build condition
  condition <- build_nested_split_condition(split_info$primary)

  if (na_propagate) {
    missing <- build_nested_split_missing(split_info$primary)
    return(expr(case_when(
      !!missing ~ NA,
      !!condition ~ !!left_subtree,
      .default = !!right_subtree
    )))
  }

  expr(case_when(!!condition ~ !!left_subtree, .default = !!right_subtree))
}

# Does this split's own column carry a missing value?
#
# Tested on the columns rather than on the split condition, because the two do
# not agree: `NA %in% c("a", "b")` is `FALSE`, not `NA`, so a categorical split
# would silently send a missing value down the right branch. The `.default` arm
# of `case_when()` does the same for a missing numeric comparison, which is the
# mechanism behind the whole family of missing-value mispredictions.
build_nested_split_missing <- function(split) {
  cols <- if (isTRUE(split$is_oblique)) unlist(split$cols) else split$col
  reduce_or(map(cols, \(col) expr(is.na(!!rlang::sym(col)))))
}

#' Build a split condition expression for nested trees (left branch)
#'
#' @param split A split info list with col, val/vals, is_categorical
#' @keywords internal
build_nested_split_condition <- function(split) {
  if (isTRUE(split$is_oblique)) {
    # Oblique split: sum(coef * col) <= threshold
    terms <- map2(
      split$coefs,
      split$cols,
      function(coef, col) expr_multiplication(coef, rlang::sym(col))
    )
    lincomb <- reduce_addition(terms)
    return(expr(!!lincomb <= !!split$val))
  }

  col <- rlang::sym(split$col)

  if (split$is_categorical) {
    # Categorical split: x %in% c("a", "b")
    vals <- unlist(split$vals)
    expr(!!col %in% !!vals)
  } else if (isTRUE(split$strict)) {
    # Continuous split where the left branch is strictly less than the
    # threshold, as `rpart` does
    expr(!!col < !!split$val)
  } else {
    # Continuous split: x <= threshold (left branch)
    expr(!!col <= !!split$val)
  }
}

# For {orbital}
#' Build nested case_when expression from tree info
#'
#' Shared helper for building nested tree expressions. This is the nested
#' equivalent of `.build_case_when_tree()`.
#'
#' @param tree_info A tree info list with nodeID, leftChild, rightChild,
#'   splitvarName, terminal, prediction, and node_splits
#' @keywords internal
#' @export
.build_nested_case_when_tree <- function(tree_info) {
  generate_nested_case_when_tree(tree_info)
}

# Build nested case_when from flat paths format
#
# Converts a flat list of leaf paths into a nested case_when expression.
# Used by parsed models (xgboost, lightgbm, catboost) when reconstructing
# trees from serialized format.
#
# @param leaves List of leaves, each with `prediction` and `path`. Each path
#   element must have an `op` field indicating the branch direction.
# @param build_condition_fn Function to build a condition expression from a
#   path element. Should only build the LEFT branch condition (the right
#   branch is handled by `.default`).
# @param is_left_op Predicate taking an `op` string and returning TRUE when it
#   denotes the left branch.
#
# ## Operator naming convention
#
# Each model names its branch operators differently, and the names do not
# always mean what they say: xgboost labels the left branch "more-equal" even
# though the condition it generates is `value < threshold`. There is therefore
# no safe shared default, and every caller must state its own convention:
#
# - **xgboost**: "more-equal" is left, "less" is right
# - **lightgbm**: "less-equal" and "in" are left, "more" and "not-in" are right
# - **catboost**: "less-equal" and "equal" are left, "more" and "not-equal" are
#   right
#
# An earlier version inferred this from one shared allowlist that happened to
# contain both of xgboost's operators. Every xgboost leaf then looked like a
# left branch, the right partition was always empty, and each tree silently
# collapsed to its first leaf's prediction.
#
# The `build_condition_fn` is only called with left-branch path elements,
# so it only needs to handle left-branch operators.
build_nested_from_flat_paths <- function(
  leaves,
  build_condition_fn,
  is_left_op
) {
  if (length(leaves) == 0) {
    cli::cli_abort("Empty tree.", .internal = TRUE)
  }

  # Single leaf (stump)
  if (length(leaves) == 1 && length(leaves[[1]]$path) == 0) {
    return(leaves[[1]]$prediction)
  }

  build_nested_from_paths_recursive(
    leaves,
    build_condition_fn,
    is_left_op,
    path_depth = 1
  )
}

build_nested_from_paths_recursive <- function(
  leaves,
  build_condition_fn,
  is_left_op,
  path_depth
) {
  if (length(leaves) == 1) {
    return(leaves[[1]]$prediction)
  }

  first_leaf <- leaves[[1]]
  if (path_depth > length(first_leaf$path)) {
    return(first_leaf$prediction)
  }

  # Partition leaves by left vs right condition based on operator name.
  # See "Operator naming convention" in build_nested_from_flat_paths docs.
  is_left_condition <- function(leaf) {
    if (path_depth > length(leaf$path)) {
      return(TRUE)
    }
    is_left_op(leaf$path[[path_depth]]$op)
  }

  left_leaves <- Filter(is_left_condition, leaves)
  right_leaves <- Filter(Negate(is_left_condition), leaves)

  # A split that sends every leaf the same way carries no information, so
  # descend past it. This is expected for unbalanced trees, where a leaf with a
  # shorter path counts as left at depths it does not reach.
  if (length(left_leaves) == 0 || length(right_leaves) == 0) {
    return(build_nested_from_paths_recursive(
      leaves,
      build_condition_fn,
      is_left_op,
      path_depth + 1
    ))
  }

  # Describe the split using a left leaf that actually reaches this depth. The
  # left and right elements carry the same column and threshold, but flags such
  # as xgboost's `missing` are stated relative to the branch taken, so reading
  # them off a right leaf would send missing values the wrong way.
  deep_enough <- Filter(\(leaf) path_depth <= length(leaf$path), left_leaves)
  split_leaf <- if (length(deep_enough) > 0) deep_enough[[1]] else first_leaf
  condition <- build_condition_fn(split_leaf$path[[path_depth]])

  left_subtree <- build_nested_from_paths_recursive(
    left_leaves,
    build_condition_fn,
    is_left_op,
    path_depth + 1
  )
  right_subtree <- build_nested_from_paths_recursive(
    right_leaves,
    build_condition_fn,
    is_left_op,
    path_depth + 1
  )

  expr(case_when(!!condition ~ !!left_subtree, .default = !!right_subtree))
}
