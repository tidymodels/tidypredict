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
#' @keywords internal

#' Generate nested case_when for multiple trees
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

  # Get split condition
  split_info <- tree_info$node_splits[[node_idx]]
  condition <- build_nested_split_condition(split_info$primary)

  # Recursively build subtrees
  left_subtree <- build_nested_node(left_id, tree_info)
  right_subtree <- build_nested_node(right_id, tree_info)

  # Build nested case_when
  expr(case_when(!!condition ~ !!left_subtree, .default = !!right_subtree))
}

#' Build a split condition expression for nested trees
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
