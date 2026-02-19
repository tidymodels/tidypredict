# Model parser ------------------------------------
get_ra_path <- function(node_id, tree, child_info, default_op = TRUE) {
  find <- node_id
  path <- node_id

  leftChild <- tree$leftChild
  rightChild <- tree$rightChild

  splitval <- tree$splitval
  splitclass <- tree$splitclass
  splitvarName <- tree$splitvarName

  # Handle stump trees (no splits) - return empty path
  if (length(child_info) == 0 || find < 1 || find > length(child_info)) {
    return(list())
  }

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
          path = get_ra_path(.x, tree, child_info, FALSE)
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
          path = get_ra_path(.x, tree, child_info, FALSE)
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
  tidypredict_fit_ranger(parsedmodel)
}

tidypredict_fit_ranger <- function(parsedmodel) {
  # Check if this is a classification model (string predictions)
  first_pred <- parsedmodel$trees[[1]][[1]]$prediction
  if (is.character(first_pred)) {
    cli::cli_abort(
      c(
        "Classification models are not supported for ranger.",
        i = "Only regression models can be converted to tidy formulas.",
        i = "Classification requires a voting mechanism that cannot be expressed as a single formula."
      )
    )
  }

  res <- generate_case_when_trees(parsedmodel)
  res <- reduce_addition(res)
  n_trees <- length(parsedmodel$trees)
  expr_division(res, n_trees)
}

# For {orbital}
#' Extract classification probability trees for ranger models
#'
#' For use in orbital package.
#' @param model A ranger model object fitted with `probability = TRUE`
#' @keywords internal
#' @export
.extract_ranger_classprob <- function(model) {
  if (!inherits(model, "ranger")) {
    cli::cli_abort(
      "{.arg model} must be {.cls ranger}, not {.obj_type_friendly {model}}."
    )
  }

  parsedmodel <- parse_model(model)

  # Check if this is a classification model with probabilities
  first_node <- parsedmodel$trees[[1]][[1]]
  if (is.null(first_node$probs)) {
    cli::cli_abort(
      c(
        "Model does not contain probability information.",
        i = "Fit the ranger model with {.code probability = TRUE}."
      )
    )
  }

  # Get class levels from the first node's probs
  lvls <- names(first_node$probs)

  # For each class, generate case_when expressions for all trees
  res <- list()
  for (lvl in lvls) {
    tree_exprs <- map(parsedmodel$trees, function(tree) {
      # Build nodes for this tree with probability of this class as prediction
      nodes <- map(tree, function(node) {
        prob <- node$probs[[lvl]]$prob
        list(
          prediction = prob,
          path = node$path
        )
      })

      # Generate case_when for this tree
      node_exprs <- map(nodes, function(node) {
        rcl <- path_formulas(node$path)
        if (isTRUE(rcl)) {
          return(node$prediction)
        }
        expr(!!rcl ~ !!node$prediction)
      })

      # Handle stump trees
      if (length(node_exprs) == 1 && is.numeric(node_exprs[[1]])) {
        return(node_exprs[[1]])
      }

      default <- node_exprs[[length(node_exprs)]]
      if (rlang::is_formula(default)) {
        default <- rlang::f_rhs(default)
      }
      node_exprs[[length(node_exprs)]] <- NULL
      node_exprs <- c(node_exprs, .default = default)

      expr(case_when(!!!node_exprs))
    })
    res[[lvl]] <- tree_exprs
  }

  res
}
