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
tidypredict_fit.randomForest <- function(model) {
  parsedmodel <- parse_model(model)
  tidypredict_fit_randomForest(parsedmodel)
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

# For {orbital}
#' Extract classification vote trees for randomForest models
#'
#' For use in orbital package.
#' @param model A randomForest model object
#' @keywords internal
#' @export
.extract_rf_classprob <- function(model) {
  if (!inherits(model, "randomForest")) {
    cli::cli_abort(
      "{.arg model} must be {.cls randomForest}, not {.obj_type_friendly {model}}."
    )
  }

  parsedmodel <- parse_model(model)

  # Check if this is a classification model (string predictions)
  first_pred <- parsedmodel$trees[[1]][[1]]$prediction
  if (!is.character(first_pred)) {
    cli::cli_abort(
      c(
        "Model is not a classification model.",
        i = "Use {.fn tidypredict_fit} for regression models."
      )
    )
  }

  # Get class levels from the model
  lvls <- model$classes

  # For each class, generate case_when expressions for all trees
  # Each tree returns 1 if it predicts the class, 0 otherwise (voting)
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
