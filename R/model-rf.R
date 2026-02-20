# Model parser -------------------------------------

parse_rf_path <- function(row_id, tree, columns, default_op = TRUE) {
  find <- row_id
  path <- row_id
  for (j in row_id:1) {
    dir <- NULL
    if (tree[j, "left daughter"] == find || tree[j, "right daughter"] == find) {
      find <- j
      path <- c(path, j)
    }
  }
  purrr::map2(
    path[1:length(path) - 1],
    path[2:length(path)],
    ~ {
      rb <- tree[.y, ]
      if (default_op) {
        if (rb["left daughter"] == .x) {
          op <- "less"
        }
        if (rb["right daughter"] == .x) {
          op <- "more-equal"
        }
      } else {
        if (rb["left daughter"] == .x) {
          op <- "less-equal"
        }
        if (rb["right daughter"] == .x) {
          op <- "more"
        }
      }
      list(
        type = "conditional",
        col = columns[rb["split var"]],
        val = rb["split point"][[1]],
        op = op
      )
    }
  )
}

parse_rf_tree <- function(tree_no, model) {
  predictions <- model$classes
  term_labels <- names(model$forest$ncat)
  tree <- randomForest::getTree(model, tree_no)
  paths <- seq_len(nrow(tree))[tree[, "status"] == -1]
  purrr::map(
    paths,
    ~ {
      if (is.null(predictions)) {
        # Classification
        prediction <- tree[.x, "prediction"]
      } else {
        # Regression
        prediction <- predictions[tree[.x, "prediction"]]
      }
      list(
        prediction = prediction,
        path = parse_rf_path(.x, tree, term_labels, default_op = FALSE)
      )
    }
  )
}

get_rf_trees <- function(model) {
  purrr::map(
    seq_len(model$ntree),
    ~ parse_rf_tree(.x, model)
  )
}

#' @export
parse_model.randomForest <- function(model) {
  classes <- attr(model$terms, "dataClasses")
  pm <- list()
  pm$general$model <- "randomForest"
  pm$general$type <- "tree"
  pm$general$version <- 2
  pm$trees <- get_rf_trees(model)
  as_parsed_model(pm)
}

# Fit model -----------------------------------------------

#' @export
tidypredict_fit.randomForest <- function(model, nested = FALSE, ...) {
  if (nested) {
    tidypredict_fit_rf_nested(model)
  } else {
    parsedmodel <- parse_model(model)
    tidypredict_fit_randomForest(parsedmodel)
  }
}

tidypredict_fit_randomForest <- function(parsedmodel) {
  # Check if this is a classification model (string predictions)
  first_pred <- parsedmodel$trees[[1]][[1]]$prediction
  if (is.character(first_pred)) {
    cli::cli_abort(
      c(
        "Classification models are not supported for randomForest.",
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

# Nested formula builder for randomForest
tidypredict_fit_rf_nested <- function(model) {
  # Check if this is a classification model
  if (!is.null(model$classes)) {
    cli::cli_abort(
      c(
        "Classification models are not supported for randomForest.",
        i = "Only regression models can be converted to tidy formulas.",
        i = "Classification requires a voting mechanism that cannot be expressed as a single formula."
      )
    )
  }

  n_trees <- model$ntree
  term_labels <- names(model$forest$ncat)

  tree_exprs <- map(seq_len(n_trees), function(tree_no) {
    build_nested_rf_tree(model, tree_no, term_labels)
  })

  res <- reduce_addition(tree_exprs)
  expr_division(res, n_trees)
}

# Build nested case_when for a single randomForest tree
build_nested_rf_tree <- function(model, tree_no, term_labels) {
  tree <- randomForest::getTree(model, tree_no)
  build_nested_rf_node(1L, tree, term_labels)
}

# Recursively build nested case_when for randomForest node
build_nested_rf_node <- function(node_id, tree, term_labels) {
  row <- tree[node_id, ]

  # Check if terminal (leaf) node - status == -1
  if (row["status"] == -1) {
    return(unname(row["prediction"]))
  }

  # Internal node - get split info
  left_id <- unname(row["left daughter"])
  right_id <- unname(row["right daughter"])
  split_var <- unname(row["split var"])
  split_val <- unname(row["split point"])

  # Recurse
  left_subtree <- build_nested_rf_node(left_id, tree, term_labels)
  right_subtree <- build_nested_rf_node(right_id, tree, term_labels)

  col_name <- term_labels[split_var]
  col_sym <- rlang::sym(col_name)

  # Numeric split: left = <= splitval, right = > splitval
  condition <- expr(!!col_sym <= !!split_val)

  expr(case_when(!!condition ~ !!left_subtree, .default = !!right_subtree))
}

# For {orbital}
#' Extract classification vote trees for randomForest models
#'
#' For use in orbital package.
#' @param model A randomForest model object
#' @param nested Logical, whether to use nested case_when (default FALSE)
#' @keywords internal
#' @export
.extract_rf_classprob <- function(model, nested = FALSE) {
  if (!inherits(model, "randomForest")) {
    cli::cli_abort(
      "{.arg model} must be {.cls randomForest}, not {.obj_type_friendly {model}}."
    )
  }

  # Check if this is a classification model
  if (is.null(model$classes)) {
    cli::cli_abort(
      c(
        "Model is not a classification model.",
        i = "Use {.fn tidypredict_fit} for regression models."
      )
    )
  }

  # Get class levels from the model
  lvls <- model$classes
  term_labels <- names(model$forest$ncat)

  if (nested) {
    # For each class, generate nested case_when expressions for all trees
    res <- list()
    for (lvl in lvls) {
      tree_exprs <- map(seq_len(model$ntree), function(tree_no) {
        build_nested_rf_vote_tree(model, tree_no, term_labels, lvl)
      })
      res[[lvl]] <- tree_exprs
    }
    return(res)
  }

  # Flat mode (original implementation)
  parsedmodel <- parse_model(model)
  res <- list()
  for (lvl in lvls) {
    tree_exprs <- map(parsedmodel$trees, function(tree) {
      # Build nodes for this tree with 1 if predicted class matches, 0 otherwise
      nodes <- map(tree, function(node) {
        list(
          prediction = if (node$prediction == lvl) 1 else 0,
          path = node$path
        )
      })
      .build_case_when_tree(nodes)
    })
    res[[lvl]] <- tree_exprs
  }

  res
}

# Build nested case_when for randomForest voting tree
build_nested_rf_vote_tree <- function(
  model,
  tree_no,
  term_labels,
  class_level
) {
  tree <- randomForest::getTree(model, tree_no)
  build_nested_rf_vote_node(1L, tree, term_labels, model$classes, class_level)
}

# Recursively build nested case_when for randomForest voting node
build_nested_rf_vote_node <- function(
  node_id,
  tree,
  term_labels,
  classes,
  class_level
) {
  row <- tree[node_id, ]

  # Check if terminal (leaf) node - status == -1
  if (row["status"] == -1) {
    # Return 1 if prediction matches class_level, 0 otherwise
    pred_class <- classes[unname(row["prediction"])]
    return(if (pred_class == class_level) 1 else 0)
  }

  # Internal node - get split info
  left_id <- unname(row["left daughter"])
  right_id <- unname(row["right daughter"])
  split_var <- unname(row["split var"])
  split_val <- unname(row["split point"])

  # Recurse
  left_subtree <- build_nested_rf_vote_node(
    left_id,
    tree,
    term_labels,
    classes,
    class_level
  )
  right_subtree <- build_nested_rf_vote_node(
    right_id,
    tree,
    term_labels,
    classes,
    class_level
  )

  col_name <- term_labels[split_var]
  col_sym <- rlang::sym(col_name)

  # Numeric split: left = <= splitval, right = > splitval
  condition <- expr(!!col_sym <= !!split_val)

  expr(case_when(!!condition ~ !!left_subtree, .default = !!right_subtree))
}
