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
#' NA values in predictor columns are not handled by the generated expression.
#' Users should ensure that predictor columns do not contain NA values before
#' using the generated expression, or the results will be NA for those rows.
#'
#' @keywords internal

#' Generate nested case_when for a tree
#'
#' @param tree_info A tree info list from `rpart_tree_info_full()` or similar
#' @keywords internal
generate_nested_case_when_tree <- function(tree_info) {
  build_nested_node(0L, tree_info)
}

#' Build a nested case_when expression for a single node
#'
#' @param node_id The node ID to build (0-indexed)
#' @param tree_info Tree info list with nodeID, leftChild, rightChild,
#'   splitvarName, terminal, prediction, and node_splits
#' @keywords internal
build_nested_node <- function(node_id, tree_info) {
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
  left_subtree <- build_nested_node(left_id, tree_info)
  right_subtree <- build_nested_node(right_id, tree_info)

  # Build condition
  condition <- build_nested_split_condition(split_info$primary)
  expr(case_when(!!condition ~ !!left_subtree, .default = !!right_subtree))
}

#' Build a split condition expression for nested trees (left branch)
#'
#' @param split A split info list with col, val/vals, is_categorical
#' @keywords internal
build_nested_split_condition <- function(split) {
  col <- rlang::sym(split$col)

  if (split$is_categorical) {
    # Categorical split: x %in% c("a", "b")
    vals <- unlist(split$vals)
    expr(!!col %in% !!vals)
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
#
# ## Operator naming convention
#
# Different models use different operator names, but they follow a pattern:
# - **Left branch operators** (condition is TRUE): "less", "less-equal", "in",
#   "more-equal", "equal"
# - **Right branch operators** (condition is FALSE): "more", "not-in",
#   "not-equal"
#
# Model-specific conventions:
# - **xgboost**: "less" and "more-equal" for left, inverse for right
# - **lightgbm**: "less-equal" and "in" for left, "more" and "not-in" for right
# - **catboost**: "less-equal" and "equal" for left, "more" and "not-equal" for right
#
# The `build_condition_fn` is only called with left-branch path elements,
# so it only needs to handle left-branch operators.
build_nested_from_flat_paths <- function(leaves, build_condition_fn) {
  if (length(leaves) == 0) {
    cli::cli_abort("Empty tree.", .internal = TRUE)
  }

  # Single leaf (stump)
  if (length(leaves) == 1 && length(leaves[[1]]$path) == 0) {
    return(leaves[[1]]$prediction)
  }

  build_nested_from_paths_recursive(leaves, build_condition_fn, path_depth = 1)
}

build_nested_from_paths_recursive <- function(
  leaves,
  build_condition_fn,
  path_depth
) {
  if (length(leaves) == 1) {
    return(leaves[[1]]$prediction)
  }

  first_leaf <- leaves[[1]]
  if (path_depth > length(first_leaf$path)) {
    return(first_leaf$prediction)
  }

  # Get condition at this depth
  split_info <- first_leaf$path[[path_depth]]

  # Partition leaves by left vs right condition based on operator name.
  # See "Operator naming convention" in build_nested_from_flat_paths docs.
  is_left_condition <- function(leaf) {
    if (path_depth > length(leaf$path)) {
      return(TRUE)
    }
    op <- leaf$path[[path_depth]]$op
    op %in% c("less", "less-equal", "in", "more-equal", "equal")
  }

  left_leaves <- Filter(is_left_condition, leaves)
  right_leaves <- Filter(Negate(is_left_condition), leaves)

  if (length(left_leaves) == 0 || length(right_leaves) == 0) {
    return(build_nested_from_paths_recursive(
      leaves,
      build_condition_fn,
      path_depth + 1
    ))
  }

  condition <- build_condition_fn(split_info)

  left_subtree <- build_nested_from_paths_recursive(
    left_leaves,
    build_condition_fn,
    path_depth + 1
  )
  right_subtree <- build_nested_from_paths_recursive(
    right_leaves,
    build_condition_fn,
    path_depth + 1
  )

  expr(case_when(!!condition ~ !!left_subtree, .default = !!right_subtree))
}
