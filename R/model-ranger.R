# Model parser ------------------------------------
get_ra_path <- function(node_id, tree, child_info, default_op = TRUE) {
  find <- node_id
  path <- node_id

  leftChild <- tree$leftChild
  rightChild <- tree$rightChild
  splitval <- tree$splitval
  splitclass <- tree$splitclass
  splitvarName <- tree$splitvarName

  new <- child_info[[find]]
  path <- find
  repeat {
    if (new == 0) {
      path <- c(path, 0)
      break
    }
    path <- c(path, new)
    find <- new
    new <- child_info[[find]]
  }

  map2(
    path[1:length(path) - 1],
    path[2:length(path)],
    ~ {
      lc <- leftChild[.y + 1] == .x
      lr <- rightChild[.y + 1] == .x
      if (is.na(splitval[.y + 1])) {
        if (lc) {
          op <- "in"
        }
        if (lr) {
          op <- "not-in"
        }
        vals <- strsplit(as.character(splitclass[.y + 1]), ", ")[[1]]
        list(
          type = "set",
          col = as.character(splitvarName[.y + 1]),
          vals = map(vals, ~.x),
          op = op
        )
      } else {
        if (default_op) {
          if (lc) {
            op <- "less"
          }
          if (lr) {
            op <- "more-equal"
          }
        } else {
          if (lc) {
            op <- "less-equal"
          }
          if (lr) {
            op <- "more"
          }
        }
        list(
          type = "conditional",
          col = as.character(splitvarName[.y + 1]),
          val = splitval[.y + 1],
          op = op
        )
      }
    }
  )
}

get_child_info <- function(tree) {
  child_info <- numeric(max(tree$nodeID))
  left_child <- tree$leftChild
  right_child <- tree$rightChild
  node_id <- tree$nodeID

  for (i in seq_len(nrow(tree))) {
    node <- node_id[[i]]

    child <- left_child[[i]]
    if (!is.na(child)) {
      child_info[child] <- node
    }

    child <- right_child[[i]]
    if (!is.na(child)) {
      child_info[child] <- node
    }
  }

  child_info
}

get_ra_tree <- function(tree_no, model) {
  tree <- ranger::treeInfo(model, tree_no)
  paths <- tree$nodeID[tree[, "terminal"]]

  child_info <- get_child_info(tree)

  map(
    paths,
    ~ {
      prediction <- tree$prediction[tree$nodeID == .x]
      if (!is.null(prediction)) {
        if (is.factor(prediction)) {
          prediction <- as.character(prediction)
        }
        list(
          prediction = prediction,
          path = get_ra_path(.x, tree, child_info, TRUE)
        )
      } else {
        preds <- map_lgl(colnames(tree), ~ "pred." == substr(.x, 1, 5))
        preds_table <- tree[tree$nodeID == .x, preds]
        predictors <- map_chr(colnames(preds_table), ~ substr(.x, 6, nchar(.x)))
        colnames(preds_table) <- predictors
        predictions <- map(preds_table, ~.x)
        max_pred <- map_lgl(predictions, ~ .x == max(map_dbl(predictions, ~.x)))
        prediction <- names(predictions)[max_pred]
        prediction <- prediction[[1]]
        prob <- predictions[max_pred]
        prob <- prob[[1]]
        predictions <- imap(predictions, ~ list(pred = .y, prob = .x))
        list(
          prediction = prediction,
          prob = prob,
          probs = predictions,
          path = get_ra_path(.x, tree, child_info, TRUE)
        )
      }
    }
  )
}

get_ra_trees <- function(model) {
  map(
    seq_len(model$num.trees),
    ~ get_ra_tree(.x, model)
  )
}

#' @export
parse_model.ranger <- function(model) {
  classes <- attr(model$terms, "dataClasses")
  pm <- list()
  pm$general$model <- "ranger"
  pm$general$type <- "tree"
  pm$general$version <- 2
  pm$trees <- get_ra_trees(model)
  as_parsed_model(pm)
}

# Fit formula -----------------------------------

#' @export
tidypredict_fit.ranger <- function(model) {
  parsedmodel <- parse_model(model)
  res <- generate_case_when_trees(parsedmodel)
  reduce_addition(res)
}
