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

  pm$trees <- get_catboost_trees(oblivious_trees, float_features)

  as_parsed_model(pm)
}

get_catboost_trees <- function(oblivious_trees, float_features) {
  map(oblivious_trees, get_catboost_tree, float_features = float_features)
}

get_catboost_tree <- function(tree_json, float_features) {
  splits <- tree_json$splits
  leaf_values <- unlist(tree_json$leaf_values)

  n_splits <- length(splits)

  # Handle stump (no splits, single leaf)
  if (n_splits == 0) {
    return(list(list(
      prediction = leaf_values[1],
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

    list(
      prediction = leaf_values[leaf_idx + 1L], # 1-indexed in R
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
  all_supported <- c(identity_objectives, sigmoid_objectives)

  if (!objective %in% all_supported) {
    cli::cli_abort(
      c(
        "Unsupported objective: {.val {objective}}.",
        "i" = "Supported objectives: {.val {all_supported}}."
      )
    )
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
