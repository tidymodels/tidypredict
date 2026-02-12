# Model parser -------------------------------------

#' @export
parse_model.catboost.Model <- function(model) {
  pm <- list()
  pm$general$model <- "catboost.Model"
  pm$general$type <- "catboost"
  pm$general$version <- 1

  tmp_file <- tempfile(fileext = ".json")
  on.exit(unlink(tmp_file), add = TRUE)
  catboost::catboost.save_model(model, tmp_file, file_format = "json")
  model_json <- jsonlite::fromJSON(tmp_file, simplifyVector = FALSE)

  pm$general$params <- list()
  loss_fn <- model_json$model_info$params$loss_function
  if (!is.null(loss_fn)) {
    if (is.list(loss_fn) && !is.null(loss_fn$type)) {
      pm$general$params$objective <- loss_fn$type
    } else {
      pm$general$params$objective <- loss_fn
    }
  }

  # Extract num_class for multiclass models
  # Only set num_class > 1 for actual multiclass objectives
  multiclass_objectives <- c("MultiClass", "MultiClassOneVsAll")
  objective <- pm$general$params$objective
  class_params <- model_json$model_info$class_params
  if (
    !is.null(objective) &&
      objective %in% multiclass_objectives &&
      !is.null(class_params) &&
      !is.null(class_params$class_names)
  ) {
    pm$general$num_class <- length(class_params$class_names)
  } else {
    pm$general$num_class <- 1
  }

  features_info <- model_json$features_info
  float_features <- features_info$float_features
  feature_names <- vapply(
    float_features,
    function(f) f$feature_id %||% paste0("feature_", f$flat_feature_index),
    character(1)
  )
  pm$general$feature_names <- feature_names
  pm$general$nfeatures <- length(feature_names)

  oblivious_trees <- model_json$oblivious_trees
  pm$general$niter <- length(oblivious_trees)

  # Extract scale and bias
  scale_and_bias <- model_json$scale_and_bias
  if (!is.null(scale_and_bias)) {
    pm$general$scale <- scale_and_bias[[1]]
    pm$general$bias <- scale_and_bias[[2]][[1]]
  } else {
    pm$general$scale <- 1
    pm$general$bias <- 0
  }

  num_class <- pm$general$num_class
  pm$trees <- get_catboost_trees(oblivious_trees, float_features, num_class)

  as_parsed_model(pm)
}

get_catboost_trees <- function(oblivious_trees, float_features, num_class) {
  if (num_class <= 1) {
    # Single output: one tree per iteration
    map(
      oblivious_trees,
      get_catboost_tree,
      float_features = float_features,
      class_idx = NULL,
      num_class = 1
    )
  } else {
    # Multiclass: create num_class virtual trees per iteration
    # Trees are stored round-robin by class: tree 1 class 0, tree 1 class 1, ...
    trees <- list()
    for (i in seq_along(oblivious_trees)) {
      for (class_idx in seq_len(num_class)) {
        trees <- c(
          trees,
          list(get_catboost_tree(
            oblivious_trees[[i]],
            float_features,
            class_idx = class_idx,
            num_class = num_class
          ))
        )
      }
    }
    trees
  }
}

get_catboost_tree <- function(
  tree_json,
  float_features,
  class_idx = NULL,
  num_class = 1
) {
  splits <- tree_json$splits
  leaf_values <- unlist(tree_json$leaf_values)

  n_splits <- length(splits)

  # Handle stump (no splits, single leaf)
  if (n_splits == 0) {
    # For multiclass, leaf_values has num_class values per leaf
    if (!is.null(class_idx) && num_class > 1) {
      pred_value <- leaf_values[class_idx]
    } else {
      pred_value <- leaf_values[1]
    }
    return(list(list(
      prediction = pred_value,
      path = list()
    )))
  }

  n_leaves <- 2^n_splits

  map(seq_len(n_leaves) - 1L, function(leaf_idx) {
    path <- vector("list", n_splits)

    for (split_idx in seq_len(n_splits)) {
      split <- splits[[split_idx]]
      # Bit position in leaf index (0-indexed from LSB)
      bit_pos <- split_idx - 1L
      # Get bit value: 1 means left (>), 0 means right (<=)
      bit_val <- bitwAnd(bitwShiftR(leaf_idx, bit_pos), 1L)

      # Get feature info
      feature_index <- split$float_feature_index + 1L # Convert to 1-indexed
      feature_info <- float_features[[feature_index]]
      feature_name <- feature_info$feature_id %||%
        paste0("feature_", feature_info$flat_feature_index)

      # Get border value
      border <- split$border

      # Determine operator based on bit value
      # CatBoost: > border goes left (bit=1), <= border goes right (bit=0)
      if (bit_val == 1L) {
        op <- "more"
      } else {
        op <- "less-equal"
      }

      # Handle NaN treatment
      nan_treatment <- feature_info$nan_value_treatment %||% "AsIs"
      missing_with_us <- get_catboost_missing(nan_treatment, op)

      path[[split_idx]] <- list(
        type = "conditional",
        col = feature_name,
        val = border,
        op = op,
        missing = missing_with_us
      )
    }

    # For multiclass, leaf_values has num_class values per leaf
    # Layout: [leaf0_class0, leaf0_class1, ..., leaf1_class0, leaf1_class1, ...]
    if (!is.null(class_idx) && num_class > 1) {
      value_idx <- leaf_idx * num_class + class_idx
    } else {
      value_idx <- leaf_idx + 1L # 1-indexed in R
    }

    list(
      prediction = leaf_values[value_idx],
      path = path
    )
  })
}

get_catboost_missing <- function(nan_treatment, op) {
  # nan_treatment determines where NaN values go:
  # - "AsIs": No special treatment, NaN doesn't match anything -> FALSE
  # - "AsTrue": NaN goes left (where condition > border is TRUE) -> TRUE if op="more"
  # - "AsFalse": NaN goes right (where condition is FALSE) -> TRUE if op="less-equal"
  if (nan_treatment == "AsTrue") {
    return(op == "more")
  } else if (nan_treatment == "AsFalse") {
    return(op == "less-equal")
  } else {
    # "AsIs" or unknown
    return(FALSE)
  }
}

# Fit model -----------------------------------------------

#' @export
tidypredict_fit.catboost.Model <- function(model) {
  parsedmodel <- parse_model(model)
  build_fit_formula_catboost(parsedmodel)
}

build_fit_formula_catboost <- function(parsedmodel) {
  n_trees <- length(parsedmodel$trees)

  if (n_trees == 0) {
    cli::cli_abort("Model has no trees.")
  }

  objective <- parsedmodel$general$params$objective
  if (is.null(objective)) {
    objective <- "RMSE"
  }

  identity_objectives <- c("RMSE", "MAE", "Quantile", "MAPE", "Poisson")
  sigmoid_objectives <- c("Logloss", "CrossEntropy")
  multiclass_objectives <- c("MultiClass", "MultiClassOneVsAll")
  all_supported <- c(
    identity_objectives,
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
    return(build_fit_formula_catboost_multiclass(parsedmodel, objective))
  }

  tree_formulas <- map(
    seq_len(n_trees),
    function(i) expr(case_when(!!!get_catboost_case_tree(i, parsedmodel)))
  )
  f <- reduce_addition(tree_formulas)

  scale <- parsedmodel$general$scale %||% 1
  bias <- parsedmodel$general$bias %||% 0

  if (scale != 1) {
    f <- expr(!!scale * (!!f))
  }
  if (bias != 0) {
    f <- expr(!!f + !!bias)
  }

  if (objective %in% sigmoid_objectives) {
    f <- expr(1 / (1 + exp(-(!!f))))
  }

  f
}

build_fit_formula_catboost_multiclass <- function(parsedmodel, objective) {
  n_trees <- length(parsedmodel$trees)
  num_class <- parsedmodel$general$num_class

  if (is.null(num_class) || num_class < 2) {
    cli::cli_abort("Multiclass model must have num_class >= 2.")
  }

  # Trees are stored round-robin by class: tree1_class0, tree1_class1, ...,
  # tree2_class0, tree2_class1, ...
  # Group trees by class: tree i belongs to class (i-1) %% num_class
  class_trees <- lapply(seq_len(num_class), function(class_idx) {
    which((seq_len(n_trees) - 1) %% num_class == (class_idx - 1))
  })

  raw_scores <- lapply(
    class_trees,
    build_catboost_tree_sum,
    parsedmodel = parsedmodel
  )

  scale <- parsedmodel$general$scale %||% 1
  bias <- parsedmodel$general$bias %||% 0

  if (scale != 1 || bias != 0) {
    raw_scores <- lapply(raw_scores, function(f) {
      if (scale != 1) {
        f <- expr(!!scale * (!!f))
      }
      if (bias != 0) {
        f <- expr(!!f + !!bias)
      }
      f
    })
  }

  # Apply transformation based on objective
  if (objective == "MultiClass") {
    # Softmax: exp(raw_i) / sum(exp(raw_j))
    exp_raws <- map(raw_scores, ~ expr(exp(!!.x)))
    denom <- reduce_addition(exp_raws)

    result <- map(seq_len(num_class), function(i) {
      expr(exp(!!raw_scores[[i]]) / (!!denom))
    })
  } else if (objective == "MultiClassOneVsAll") {
    # One-vs-all: sigmoid for each class independently
    result <- map(raw_scores, ~ expr(1 / (1 + exp(-(!!.x)))))
  }

  names(result) <- paste0("class_", seq_len(num_class) - 1)
  result
}

build_catboost_tree_sum <- function(tree_indices, parsedmodel) {
  if (length(tree_indices) == 0) {
    cli::cli_abort("No trees found for tree indices.", .internal = TRUE)
  }
  tree_formulas <- map(
    tree_indices,
    function(i) expr(case_when(!!!get_catboost_case_tree(i, parsedmodel)))
  )
  reduce_addition(tree_formulas)
}

get_catboost_case_tree <- function(tree_no, parsedmodel) {
  map(
    parsedmodel$trees[[tree_no]],
    function(leaf) get_catboost_case(leaf$path, leaf$prediction)
  )
}

get_catboost_case <- function(path, prediction) {
  cl <- map(path, get_catboost_case_fun)
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

get_catboost_case_fun <- function(.x) {
  if (.x$type != "conditional") {
    cli::cli_abort(
      "CatBoost only supports conditional splits, not {.val {.x$type}}.",
      .internal = TRUE
    )
  }

  col_name <- as.name(.x$col)
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
    cli::cli_abort(
      "Unknown operator: {.val {.x$op}}.",
      .internal = TRUE
    )
  }

  i
}

# For {orbital}
#' Extract processed CatBoost trees
#'
#' For use in orbital package.
#' @param model A CatBoost model object
#' @keywords internal
#' @export
.extract_catboost_trees <- function(model) {
  if (!inherits(model, "catboost.Model")) {
    cli::cli_abort(
      "{.arg model} must be {.cls catboost.Model}, not {.obj_type_friendly {model}}."
    )
  }

  parsedmodel <- parse_model(model)

  map(
    seq_len(length(parsedmodel$trees)),
    function(i) expr(case_when(!!!get_catboost_case_tree(i, parsedmodel)))
  )
}
