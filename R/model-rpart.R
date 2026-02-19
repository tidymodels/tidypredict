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

  rpart_left <- ifelse(
    left_candidates %in% orig_node_ids,
    id_map[as.character(left_candidates)],
    NA_integer_
  )
  rpart_right <- ifelse(
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

  # Track ncat sign to determine if we need to swap left/right
  # ncat = 1: rpart goes left if value >= split (need swap for get_ra_path)
  # ncat = -1: rpart goes left if value < split (no swap needed)
  needs_swap <- rep(FALSE, nrow(frame))

  if (!is.null(splits) && nrow(splits) > 0) {
    split_idx <- 1
    for (i in seq_len(nrow(frame))) {
      if (!is_terminal[i]) {
        ncat <- splits[split_idx, "ncat"]
        index <- splits[split_idx, "index"]

        if (abs(ncat) == 1) {
          split_val[i] <- index
          # ncat = 1 means left if >= (opposite of get_ra_path), needs swap
          # ncat = -1 means left if < (same as get_ra_path), no swap
          needs_swap[i] <- ncat == 1
        } else if (abs(ncat) > 1) {
          csplit_row <- model$csplit[index, , drop = TRUE]
          xlevels <- attr(model, "xlevels")[[split_var[i]]]
          # 1 = go left, 3 = go right, 2 = missing
          # For categorical, ncat sign indicates direction too
          if (ncat > 0) {
            left_levels <- xlevels[csplit_row == 1]
          } else {
            left_levels <- xlevels[csplit_row == 3]
          }
          split_class[i] <- paste0(left_levels, collapse = ", ")
        }

        n_splits <- 1 + frame$ncompete[i] + frame$nsurrogate[i]
        split_idx <- split_idx + n_splits
      }
    }
  }

  # Swap left/right based on ncat direction
  left_child <- ifelse(needs_swap, rpart_right, rpart_left)
  right_child <- ifelse(needs_swap, rpart_left, rpart_right)

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

#' @export
parse_model.rpart <- function(model) {
  pm <- list()
  pm$general$model <- "rpart"
  pm$general$type <- "tree"
  pm$general$version <- 2
  pm$trees <- list(get_rpart_tree(model))
  as_parsed_model(pm)
}

#' @export
tidypredict_fit.rpart <- function(model) {
  parsedmodel <- parse_model(model)
  tree <- parsedmodel$trees[[1]]
  mode <- parsedmodel$general$mode
  generate_case_when_tree(tree, mode)
}

#' @export
tidypredict_test.rpart <- function(
  model,
  df = model$model,
  threshold = 0.000000000001,
  include_intervals = FALSE,
  max_rows = NULL,
  xg_df = NULL
) {
  if (is.numeric(max_rows)) {
    df <- head(df, max_rows)
  }

  # rpart uses "vector" for regression, "class" for classification
  pred_type <- if (model$method == "class") "class" else "vector"
  preds <- predict(model, df, type = pred_type)

  if (pred_type == "class") {
    preds <- as.character(preds)
  }

  base <- data.frame(fit = as.vector(preds), row.names = NULL)

  te <- tidypredict_to_column(
    df,
    model,
    add_interval = FALSE,
    vars = c("fit_te", "upr_te", "lwr_te")
  )
  te <- data.frame(fit_te = te[, "fit_te"])

  raw_results <- cbind(base, te)

  if (pred_type == "class") {
    raw_results$fit_diff <- raw_results$fit != raw_results$fit_te
    raw_results$fit_threshold <- raw_results$fit_diff
  } else {
    raw_results$fit_diff <- raw_results$fit - raw_results$fit_te
    raw_results$fit_threshold <- abs(raw_results$fit_diff) > threshold
  }

  rowid <- seq_len(nrow(raw_results))
  raw_results <- cbind(data.frame(rowid), raw_results)

  threshold_df <- data.frame(fit_threshold = sum(raw_results$fit_threshold))
  alert <- any(threshold_df > 0)

  message <- paste0(
    "tidypredict test results\n",
    "Difference threshold: ",
    threshold,
    "\n"
  )

  if (alert) {
    if (pred_type == "class") {
      message <- paste0(
        message,
        "\nMismatched predictions: ",
        threshold_df$fit_threshold
      )
    } else {
      difference <- data.frame(fit_diff = max(abs(raw_results$fit_diff)))
      message <- paste0(
        message,
        "\nFitted records above the threshold: ",
        threshold_df$fit_threshold,
        "\n\nMax difference: ",
        difference$fit_diff
      )
    }
  } else {
    message <- paste0(
      message,
      "\n All results are within the difference threshold"
    )
  }

  results <- list()
  results$model_call <- model$call
  results$raw_results <- raw_results
  results$message <- message
  results$alert <- alert
  structure(results, class = c("tidypredict_test", "list"))
}
