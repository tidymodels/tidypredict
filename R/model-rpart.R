rpart_tree_info <- function(model) {
  frame <- model$frame
  splits <- model$splits
  orig_node_ids <- as.integer(rownames(frame))

  # Create mapping from original rpart node IDs to sequential 0-indexed IDs
  # This is needed because rpart uses sparse binary tree numbering (1,2,3,4,5,6,11,12,...)
  # but get_ra_path assumes nodeID == row_index - 1
  id_map <- setNames(seq_along(orig_node_ids) - 1L, orig_node_ids)

  # Build child relationships based on binary tree convention
  # Left child of node n is 2n, right child is 2n+1
  left_candidates <- 2L * orig_node_ids
  right_candidates <- 2L * orig_node_ids + 1L

  # Map children to new sequential IDs
  left_child <- ifelse(
    left_candidates %in% orig_node_ids,
    id_map[as.character(left_candidates)],
    NA_integer_
  )
  right_child <- ifelse(
    right_candidates %in% orig_node_ids,
    id_map[as.character(right_candidates)],
    NA_integer_
  )

  is_terminal <- frame$var == "<leaf>"

  if (model$method == "class") {
    ylevels <- attr(model, "ylevels")
    prediction <- ifelse(is_terminal, ylevels[frame$yval], NA)
  } else {
    prediction <- ifelse(is_terminal, frame$yval, NA)
  }

  split_var <- as.character(frame$var)
  split_val <- rep(NA_real_, nrow(frame))
  split_class <- rep(NA_character_, nrow(frame))

  if (!is.null(splits) && nrow(splits) > 0) {
    split_idx <- 1
    for (i in seq_len(nrow(frame))) {
      if (!is_terminal[i]) {
        ncat <- splits[split_idx, "ncat"]
        index <- splits[split_idx, "index"]

        if (abs(ncat) == 1) {
          split_val[i] <- index
        } else if (abs(ncat) > 1) {
          csplit_row <- model$csplit[index, , drop = TRUE]
          xlevels <- attr(model, "xlevels")[[split_var[i]]]
          # 1 = go left, 3 = go right, 2 = missing
          left_levels <- xlevels[csplit_row == 1]
          split_class[i] <- paste0(left_levels, collapse = ", ")
        }

        n_splits <- 1 + frame$ncompete[i] + frame$nsurrogate[i]
        split_idx <- split_idx + n_splits
      }
    }
  }

  # Use sequential 0-indexed node IDs
  data.frame(
    nodeID = seq_along(orig_node_ids) - 1L,
    leftChild = left_child,
    rightChild = right_child,
    splitvarName = split_var,
    splitval = split_val,
    splitclass = split_class,
    terminal = is_terminal,
    prediction = prediction
  )
}

get_rpart_tree <- function(model) {
  tree <- rpart_tree_info(model)
  paths <- tree$nodeID[tree[, "terminal"]]

  child_info <- get_child_info(tree)

  map(
    paths,
    \(x) {
      prediction <- tree$prediction[tree$nodeID == x]
      if (is.null(prediction)) {
        cli::cli_abort("Prediction column not found.")
      }
      if (is.factor(prediction)) {
        prediction <- as.character(prediction)
      }
      list(
        prediction = prediction,
        path = get_ra_path(x, tree, child_info, FALSE)
      )
    }
  )
}
