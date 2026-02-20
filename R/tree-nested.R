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
#' @param na_handling How to handle NA values in split variables:
#'   - `"none"` (default): No NA handling, fastest and simplest
#'   - `"surrogate"`: Use surrogate splits for NA handling (like rpart default)
#'   - `"stop"`: Predict at first NA encountered (like `usesurrogate=0`)
#' @keywords internal
generate_nested_case_when_tree <- function(tree_info, na_handling = "none") {
  na_handling <- rlang::arg_match(
    na_handling,
    values = c("none", "surrogate", "stop")
  )
  build_nested_node(0L, tree_info, na_handling)
}

#' Build a nested case_when expression for a single node
#'
#' @param node_id The node ID to build (0-indexed)
#' @param tree_info Tree info list with nodeID, leftChild, rightChild,
#'   splitvarName, terminal, prediction, and node_splits
#' @param na_handling How to handle NA values
#' @keywords internal
build_nested_node <- function(node_id, tree_info, na_handling = "none") {
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
  left_subtree <- build_nested_node(left_id, tree_info, na_handling)
  right_subtree <- build_nested_node(right_id, tree_info, na_handling)

  # Build expression based on na_handling mode
  if (na_handling == "none") {
    condition <- build_nested_split_condition(split_info$primary)
    expr(case_when(!!condition ~ !!left_subtree, .default = !!right_subtree))
  } else if (na_handling == "stop") {
    build_nested_node_stop(
      node_idx,
      tree_info,
      split_info,
      left_subtree,
      right_subtree
    )
  } else {
    # na_handling == "surrogate"
    build_nested_node_surrogate(
      node_idx,
      tree_info,
      split_info,
      left_subtree,
      right_subtree
    )
  }
}

#' Build nested node with "stop" NA handling
#'
#' When the split variable is NA, predict at this node instead of continuing.
#' @keywords internal
build_nested_node_stop <- function(
  node_idx,
  tree_info,
  split_info,
  left_subtree,
  right_subtree
) {
  primary <- split_info$primary
  col <- rlang::sym(primary$col)

  # Get prediction for this internal node (used when NA)
  node_prediction <- tree_info$prediction[node_idx]
  if (is.factor(node_prediction)) {
    node_prediction <- as.character(node_prediction)
  }

  # Build condition for left branch (not NA and satisfies condition)
  left_cond <- build_nested_split_condition(primary)

  # Structure: if NA -> predict here, if left_cond -> left, else -> right
  expr(case_when(
    is.na(!!col) ~ !!node_prediction,
    !!left_cond ~ !!left_subtree,
    .default = !!right_subtree
  ))
}

#' Build nested node with "surrogate" NA handling
#'
#' Use surrogate splits to handle NA values, with majority direction as fallback.
#' @keywords internal
build_nested_node_surrogate <- function(
  node_idx,
  tree_info,
  split_info,
  left_subtree,
  right_subtree
) {
  primary <- split_info$primary
  surrogates <- split_info$surrogates
  majority_left <- tree_info$majority_left[node_idx]

  primary_col <- rlang::sym(primary$col)

  # Build condition for going LEFT
  left_terms <- list()

  # Primary: not NA and satisfies left condition
  primary_left_cond <- build_nested_split_condition(primary)
  left_terms[[1]] <- expr(!is.na(!!primary_col) & !!primary_left_cond)

  # Track NA checks for surrogate chain
  na_checks <- list(expr(is.na(!!primary_col)))

  # Surrogates
  for (surr in surrogates) {
    surr_col <- rlang::sym(surr$col)

    # Surrogate condition (adjusted for direction)
    if (surr$needs_swap) {
      surr_cond <- build_nested_split_condition_right(surr)
    } else {
      surr_cond <- build_nested_split_condition(surr)
    }

    # All previous vars NA, this one not NA and satisfies condition
    prev_na <- reduce_and(na_checks)
    surr_term <- expr(!!prev_na & !is.na(!!surr_col) & !!surr_cond)
    left_terms <- c(left_terms, list(surr_term))

    na_checks <- c(na_checks, list(expr(is.na(!!surr_col))))
  }

  # All NA case: go to majority direction
  if (isTRUE(majority_left)) {
    all_na <- reduce_and(na_checks)
    left_terms <- c(left_terms, list(all_na))
  }

  # Combine all left conditions with OR
  left_condition <- reduce_or(left_terms)

  expr(case_when(
    !!left_condition ~ !!left_subtree,
    .default = !!right_subtree
  ))
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

#' Build a split condition expression for right branch
#'
#' @param split A split info list with col, val/vals, is_categorical
#' @keywords internal
build_nested_split_condition_right <- function(split) {
  col <- rlang::sym(split$col)

  if (split$is_categorical) {
    # Categorical split: x NOT in c("a", "b")
    vals <- unlist(split$vals)
    expr((!!col %in% !!vals) == FALSE)
  } else {
    # Continuous split: x > threshold (right branch)
    expr(!!col > !!split$val)
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
#' @param na_handling How to handle NA values: `"none"`, `"surrogate"`, or
#'   `"stop"`
#' @keywords internal
#' @export
.build_nested_case_when_tree <- function(tree_info, na_handling = "none") {
  generate_nested_case_when_tree(tree_info, na_handling)
}
