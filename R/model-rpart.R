rpart_tree_info <- function(model) {
  frame <- model$frame
  splits <- model$splits
  node_ids <- as.integer(rownames(frame))

  left_candidates <- 2L * node_ids
  right_candidates <- 2L * node_ids + 1L
  left_child <- ifelse(
    left_candidates %in% node_ids,
    left_candidates,
    NA_integer_
  )
  right_child <- ifelse(
    right_candidates %in% node_ids,
    right_candidates,
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
          # ncat sign indicates direction (1 = left if <, -1 = left if >=)
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

  # Convert to 0-indexed to match other tidypredict implementations
  data.frame(
    nodeID = node_ids - 1L,
    leftChild = left_child - 1L,
    rightChild = right_child - 1L,
    splitvarName = split_var,
    splitval = split_val,
    splitclass = split_class,
    terminal = is_terminal,
    prediction = prediction
  )
}
