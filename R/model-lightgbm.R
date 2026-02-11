# Model parser -------------------------------------

#' @export
parse_model.lgb.Booster <- function(model) {
  pm <- list()
  pm$general$model <- "lgb.Booster"
  pm$general$type <- "lgb"
  pm$general$version <- 1

  # Extract params (objective, etc.)
  pm$general$params <- model$params

  # Extract feature names and multiclass info from JSON dump
  model_json <- jsonlite::fromJSON(model$dump_model())
  pm$general$feature_names <- model_json$feature_names
  pm$general$nfeatures <- length(model_json$feature_names)
  pm$general$num_class <- model_json$num_class
  pm$general$num_tree_per_iteration <- model_json$num_tree_per_iteration

  # Extract number of iterations
  pm$general$niter <- model$current_iter()

  # Extract trees
  pm$trees <- get_lgb_trees(model)

  as_parsed_model(pm)
}

get_lgb_trees <- function(model) {
  trees_df <- lightgbm::lgb.model.dt.tree(model)
  trees_df <- as.data.frame(trees_df)

  # Check for unsupported decision types
  decision_types <- unique(trees_df$decision_type[
    !is.na(trees_df$decision_type)
  ])
  supported_types <- c("<=", "==")
  unsupported <- setdiff(decision_types, supported_types)
  if (length(unsupported) > 0) {
    # nocov start
    cli::cli_abort(
      c(
        "Unsupported decision type{?s} found: {.val {unsupported}}.",
        "i" = "Supported types: {.val {supported_types}}."
      ),
      .internal = TRUE
    )
    # nocov end
  }

  # Split by tree_index
  trees_split <- split(trees_df, trees_df$tree_index)

  # Process each tree
  map(trees_split, get_lgb_tree)
}

get_lgb_children_map <- function(tree_df) {
  # For each split_index, find its children (ordered by row index)
  split_indices <- tree_df$split_index[!is.na(tree_df$split_index)]

  children_map <- lapply(split_indices, function(si) {
    # Children are rows where node_parent==si OR leaf_parent==si
    child_rows <- which(tree_df$node_parent == si | tree_df$leaf_parent == si)
    # Sort by row index: first is LEFT, second is RIGHT
    child_rows[order(child_rows)]
  })
  names(children_map) <- as.character(split_indices)
  children_map
}

get_lgb_tree <- function(tree_df) {
  # Build children map for direction detection
  children_map <- get_lgb_children_map(tree_df)

  # Find leaf rows
  leaf_rows <- which(!is.na(tree_df$leaf_index))

  # For each leaf, trace path to root
  map(leaf_rows, function(leaf_row) {
    list(
      prediction = tree_df$leaf_value[[leaf_row]],
      path = get_lgb_path(leaf_row, tree_df, children_map)
    )
  })
}

get_lgb_path <- function(leaf_row, tree_df, children_map) {
  path <- list()
  current_row <- leaf_row
  current_parent_split <- tree_df$leaf_parent[[leaf_row]]

  while (!is.na(current_parent_split)) {
    # Find the parent's row (split_index should be unique within a tree)
    parent_row <- which(tree_df$split_index == current_parent_split)[[1]]

    # Determine direction: is current_row the LEFT or RIGHT child?
    children <- children_map[[as.character(current_parent_split)]]
    is_left_child <- (current_row == children[1])

    # Build condition based on decision type
    decision_type <- tree_df$decision_type[[parent_row]]
    default_left <- tree_df$default_left[[parent_row]] == "TRUE"

    if (decision_type == "<=") {
      # Numerical split
      if (is_left_child) {
        op <- "less-equal"
        missing_with_us <- default_left
      } else {
        op <- "more"
        missing_with_us <- !default_left
      }

      condition <- list(
        type = "conditional",
        col = tree_df$split_feature[[parent_row]],
        val = tree_df$threshold[[parent_row]],
        op = op,
        missing = missing_with_us
      )
    } else if (decision_type == "==") {
      # Categorical split: threshold is "0||1||3" format
      # LEFT = category IN set, RIGHT = category NOT IN set
      category_set <- parse_lgb_categorical_threshold(
        tree_df$threshold[[parent_row]]
      )

      if (is_left_child) {
        op <- "in"
        missing_with_us <- default_left
      } else {
        op <- "not-in"
        missing_with_us <- !default_left
      }

      condition <- list(
        type = "set",
        col = tree_df$split_feature[[parent_row]],
        vals = category_set,
        op = op,
        missing = missing_with_us
      )
    }

    path <- c(path, list(condition))

    # Move up the tree
    current_row <- parent_row
    current_parent_split <- tree_df$node_parent[[parent_row]]
  }

  rev(path) # Reverse to get root-to-leaf order
}

# Parse LightGBM categorical threshold format "0||1||3" -> c(0, 1, 3)
parse_lgb_categorical_threshold <- function(threshold) {
  as.integer(strsplit(threshold, "[|][|]")[[1]])
}

# Fit model -----------------------------------------------

#' @export
tidypredict_fit.lgb.Booster <- function(model) {
  parsedmodel <- parse_model(model)
  build_fit_formula_lgb(parsedmodel)
}

build_fit_formula_lgb <- function(parsedmodel) {
  n_trees <- length(parsedmodel$trees)

  if (n_trees == 0) {
    cli::cli_abort("Model has no trees.")
  }

  objective <- parsedmodel$general$params$objective
  if (is.null(objective)) {
    objective <- "regression"
  }

  identity_objectives <- c(
    "regression",
    "regression_l2",
    "regression_l1",
    "huber",
    "fair",
    "quantile",
    "mape"
  )
  exp_objectives <- c("poisson", "gamma", "tweedie")
  sigmoid_objectives <- c("binary", "cross_entropy")
  multiclass_objectives <- c("multiclass", "multiclassova")
  all_supported <- c(
    identity_objectives,
    exp_objectives,
    sigmoid_objectives,
    multiclass_objectives
  )

  if (!objective %in% all_supported) {
    cli::cli_abort(
      c(
        "Unsupported objective: {.val {objective}}.",
        "i" = "Supported objectives: {.val {all_supported}}."
      )
    )
  }

  # Handle multiclass separately
  if (objective %in% multiclass_objectives) {
    return(build_fit_formula_lgb_multiclass(parsedmodel, objective))
  }

  # Single output objectives: sum all trees
  f <- build_lgb_tree_sum(seq_len(n_trees), parsedmodel)

  # Apply transformation
  if (objective %in% exp_objectives) {
    f <- expr(exp(!!f))
  } else if (objective %in% sigmoid_objectives) {
    f <- lgb_sigmoid(f)
  }
  # identity_objectives: no transformation needed

  f
}

build_fit_formula_lgb_multiclass <- function(parsedmodel, objective) {
  n_trees <- length(parsedmodel$trees)
  num_class <- parsedmodel$general$num_class

  if (is.null(num_class) || num_class < 2) {
    cli::cli_abort("Multiclass model must have num_class >= 2.")
  }

  # Group trees by class: tree i belongs to class (i-1) %% num_class
  # (trees are 1-indexed in our list, but LightGBM uses 0-indexed tree_index)
  class_trees <- lapply(seq_len(num_class), function(class_idx) {
    which((seq_len(n_trees) - 1) %% num_class == (class_idx - 1))
  })

  # Build raw score formula for each class
  raw_scores <- lapply(
    class_trees,
    build_lgb_tree_sum,
    parsedmodel = parsedmodel
  )

  # Apply transformation based on objective
  if (objective == "multiclass") {
    # Softmax: exp(raw_i) / sum(exp(raw_j))
    exp_raws <- map(raw_scores, ~ expr(exp(!!.x)))
    denom <- reduce_addition(exp_raws)

    result <- map(seq_len(num_class), function(i) {
      expr(exp(!!raw_scores[[i]]) / (!!denom))
    })
  } else if (objective == "multiclassova") {
    # One-vs-all: sigmoid for each class independently
    result <- map(raw_scores, lgb_sigmoid)
  }

  names(result) <- paste0("class_", seq_len(num_class) - 1)
  result
}

# Helper to build sum of tree predictions for given indices
build_lgb_tree_sum <- function(tree_indices, parsedmodel) {
  if (length(tree_indices) == 0) {
    # nocov start
    cli::cli_abort("No trees found for tree indices.", .internal = TRUE)
    # nocov end
  }
  tree_formulas <- map(
    tree_indices,
    ~ expr(case_when(!!!get_lgb_case_tree(.x, parsedmodel)))
  )
  reduce_addition(tree_formulas)
}

# Helper for sigmoid transformation
lgb_sigmoid <- function(f) {
  expr(1 / (1 + exp(-(!!f))))
}

get_lgb_case_tree <- function(tree_no, parsedmodel) {
  map(
    parsedmodel$trees[[tree_no]],
    ~ get_lgb_case(.x$path, .x$prediction)
  )
}

get_lgb_case <- function(path, prediction) {
  cl <- map(path, get_lgb_case_fun)
  cl_length <- length(cl)

  if (cl_length == 0) {
    cl <- TRUE
  } else if (cl_length == 1) {
    cl <- cl[[1]]
  } else if (cl_length == 2) {
    cl <- expr_and(cl[[1]], cl[[2]])
  } else {
    cl <- reduce_and(cl)
  }

  expr(!!cl ~ !!prediction)
}

# For {orbital}
#' Extract processed LightGBM trees
#'
#' For use in orbital package.
#' @param model A LightGBM model object
#' @keywords internal
#' @export
.extract_lgb_trees <- function(model) {
  if (!inherits(model, "lgb.Booster")) {
    cli::cli_abort(
      "{.arg model} must be {.cls lgb.Booster}, not {.obj_type_friendly {model}}."
    )
  }

  parsedmodel <- parse_model(model)

  map(
    seq_len(length(parsedmodel$trees)),
    ~ expr(case_when(!!!get_lgb_case_tree(.x, parsedmodel)))
  )
}

get_lgb_case_fun <- function(.x) {
  col_name <- as.name(.x$col)

  if (.x$type == "conditional") {
    # Numerical split
    val <- as.numeric(.x$val)

    if (.x$op == "less-equal") {
      if (.x$missing) {
        i <- expr((!!col_name <= !!val | is.na(!!col_name)))
      } else {
        i <- expr(!!col_name <= !!val)
      }
    } else if (.x$op == "more") {
      if (.x$missing) {
        i <- expr((!!col_name > !!val | is.na(!!col_name)))
      } else {
        i <- expr(!!col_name > !!val)
      }
    } else {
      # nocov start
      cli::cli_abort(
        "Unknown operator for conditional: {.val {(.x$op)}}.",
        .internal = TRUE
      )
      # nocov end
    }
  } else if (.x$type == "set") {
    # Categorical split
    vals <- .x$vals

    if (.x$op == "in") {
      if (.x$missing) {
        i <- expr((!!col_name %in% !!vals | is.na(!!col_name)))
      } else {
        i <- expr(!!col_name %in% !!vals)
      }
    } else if (.x$op == "not-in") {
      if (.x$missing) {
        i <- expr((!(!!col_name %in% !!vals) | is.na(!!col_name)))
      } else {
        i <- expr(!(!!col_name %in% !!vals))
      }
    } else {
      cli::cli_abort("Unknown operator for set: {.val {(.x$op)}}")
    }
  } else {
    cli::cli_abort("Unknown condition type: {.val {(.x$type)}}")
  }

  i
}
