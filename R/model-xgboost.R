# Model parser -------------------------------------

get_xgb_path <- function(row_id, tree) {
  find <- row_id
  path <- row_id

  yes_vec <- tree$Yes
  no_vec <- tree$No

  for (j in row_id:1) {
    yes <- yes_vec[[j]]
    no <- no_vec[[j]]

    if (!is.na(yes) && !is.na(no)) {
      if (yes == find || no == find) {
        find <- j
        path <- c(path, j)
      }
    }
  }

  purrr::map2(
    path[seq2(1, length(path) - 1)],
    path[seq2(2, length(path))],
    get_xgb_path_fun,
    tree = tree
  )
}

get_xgb_path_fun <- function(.x, .y, tree) {
  yes <- tree$Yes[[.y]]
  no <- tree$No[[.y]]

  missing <- tree$Missing[[.y]]
  if (yes %in% .x) {
    op <- "more-equal"
    missing <- missing %in% yes
  }
  if (no %in% .x) {
    op <- "less"
    missing <- missing %in% no
  }
  list(
    type = "conditional",
    col = tree$feature_name[[.y]],
    val = tree$Split[[.y]],
    op = op,
    missing = missing
  )
}

get_xgb_tree <- function(tree) {
  paths <- seq_len(nrow(tree))[tree[, "Feature"] == "Leaf"]
  x <- map(
    paths,
    ~ {
      list(
        prediction = tree$Gain[[.x]] %||% tree$Quality[[.x]],
        path = get_xgb_path(.x, tree)
      )
    }
  )
  x
}

get_xgb_trees <- function(model) {
  xd <- xgboost::xgb.dump(
    model = model,
    dump_format = "text",
    with_stats = TRUE
  )
  if (is.null(attr(model, "param"))) {
    # Old xgboost API (< 2.0) - kept for backwards compatibility
    xd <- xgboost::xgb.dump(
      model = model,
      dump_format = "text",
      with_stats = TRUE
    )
    feature_names <- model$feature_names
    get_xgb_trees_character(xd, feature_names)
  } else {
    feature_names <- xgboost::getinfo(model, "feature_name")
    get_xgb_trees_character(model, feature_names)
  }
}

get_xgb_trees_character <- function(x, feature_names) {
  # Old xgboost API (< 2.0) - kept for backwards compatibility with old models
  if (is.character(x)) {
    trees <- xgboost::xgb.model.dt.tree(text = x)
  } else {
    trees <- xgboost::xgb.model.dt.tree(model = x)
  }
  trees <- as.data.frame(trees)
  trees$original_order <- 1:nrow(trees)

  if (is.character(x)) {
    feature_names_tbl <- data.frame(
      Feature = as.character(0:(length(feature_names) - 1)),
      feature_name = feature_names,
      stringsAsFactors = FALSE
    )
    trees <- merge(trees, feature_names_tbl, by = "Feature", all.x = TRUE)
  } else {
    trees$feature_name <- ifelse(trees$Feature == "Leaf", NA, trees$Feature)
  }
  trees <- trees[
    order(trees$original_order),
    !names(trees) %in% "original_order"
  ]
  trees[, c("Yes", "No", "Missing")] <-
    lapply(trees[, c("Yes", "No", "Missing")], function(x) sub("^.*-", "", x))
  trees[, c("Yes", "No", "Missing")] <-
    lapply(trees[, c("Yes", "No", "Missing")], function(x) as.integer(x) + 1)

  trees_split <- split(trees, trees$Tree)

  purrr::map(trees_split, get_xgb_tree)
}

#' @export
parse_model.xgb.Booster <- function(model) {
  old <- is.null(attr(model, "param"))

  params <- attr(model, "param") %||% model$params
  wosilent <- params[names(params) != "silent"]
  wosilent$silent <- params$silent

  pm <- list()
  pm$general$model <- "xgb.Booster"
  pm$general$type <- "xgb"
  pm$general$params <- wosilent

  if (old) {
    # Old xgboost API (< 2.0) - kept for backwards compatibility
    pm$general$feature_names <- model$feature_names
    pm$general$niter <- model$niter
    pm$general$nfeatures <- model$nfeatures
  } else {
    pm$general$feature_names <- xgboost::getinfo(model, "feature_name")
    pm$general$niter <- utils::getFromNamespace(
      "xgb.get.num.boosted.rounds",
      ns = "xgboost"
    )(model)
    pm$general$nfeatures <- length(pm$general$feature_names)
  }

  json_params <- get_xgb_json_params(model)
  pm$general$params$base_score <- json_params$base_score
  pm$general$booster_name <- json_params$booster_name
  pm$general$weight_drop <- json_params$weight_drop

  pm$general$version <- 3
  pm$trees <- get_xgb_trees(model)
  as_parsed_model(pm)
}

# Shared helpers -----------------------------------------------

apply_dart_weights <- function(trees, weight_drop) {
  if (!is.null(weight_drop) && !all(weight_drop == 1)) {
    trees <- Map(
      function(tree_expr, weight) {
        expr(!!tree_expr * !!weight)
      },
      trees,
      weight_drop
    )
  }
  trees
}

get_xgb_json_params <- function(model) {
  tmp_file <- tempfile(fileext = ".json")
  xgboost::xgb.save(model, tmp_file)

  # Use regex extraction instead of full JSON parsing (3-4x faster)
  txt <- paste(readLines(tmp_file, warn = FALSE), collapse = "")

  # Extract base_score - format is "base_score":"[5E-1]"
  base_score_match <- regmatches(
    txt,
    regexpr('base_score":"\\[[^]]+\\]', txt, perl = TRUE)
  )

  if (length(base_score_match) > 0 && nchar(base_score_match) > 0) {
    base_score_str <- gsub(
      'base_score":"\\[([^]]+)\\]',
      "\\1",
      base_score_match,
      perl = TRUE
    )
    base_score <- as.numeric(strsplit(base_score_str, ",")[[1]])
  } else {
    base_score <- 0.5 # nocov
  }

  # Extract booster name using fixed string matching

  booster_name <- "gbtree"
  if (grepl('"name":"dart"', txt, fixed = TRUE)) {
    booster_name <- "dart"
  } else if (grepl('"name":"gblinear"', txt, fixed = TRUE)) {
    booster_name <- "gblinear"
  }

  # Extract weight_drop for DART

  weight_drop <- NULL
  if (booster_name == "dart") {
    wd_match <- regmatches(
      txt,
      regexpr('weight_drop":\\[[^]]+\\]', txt, perl = TRUE)
    )
    if (length(wd_match) > 0 && nchar(wd_match) > 0) {
      wd_str <- gsub('weight_drop":\\[([^]]+)\\]', "\\1", wd_match, perl = TRUE)
      weight_drop <- as.numeric(strsplit(wd_str, ",")[[1]])
    }
  }

  list(
    base_score = base_score,
    booster_name = booster_name,
    weight_drop = weight_drop
  )
}

# Fit model (nested) -----------------------------------------------

#' @export
tidypredict_fit.xgb.Booster <- function(model, ...) {
  build_fit_formula_xgb_nested(model)
}

# Build nested xgboost formula (from model directly)
build_fit_formula_xgb_nested <- function(model) {
  trees_nested <- extract_xgb_trees_nested(model)

  # Apply DART weight_drop if present
  json_params <- get_xgb_json_params(model)
  trees_nested <- apply_dart_weights(trees_nested, json_params$weight_drop)

  # Additive model
  f <- reduce_addition(trees_nested)

  base_score <- json_params$base_score
  if (is.null(base_score)) {
    base_score <- 0.5 # nocov
  }

  params <- attr(model, "param") %||% model$params
  objective <- params$objective

  apply_xgb_objective(f, objective, base_score)
}

# Build nested formula from parsed xgboost model (version 3)
build_fit_formula_xgb_from_parsed <- function(parsedmodel) {
  # Build nested trees from flat paths
  trees_nested <- map(parsedmodel$trees, function(tree) {
    build_nested_from_flat_paths(tree, build_xgb_nested_condition)
  })

  # Apply DART weight_drop if present
  weight_drop <- parsedmodel$general$weight_drop
  trees_nested <- apply_dart_weights(trees_nested, weight_drop)

  # Additive model
  f <- reduce_addition(trees_nested)

  base_score <- parsedmodel$general$params$base_score
  if (is.null(base_score)) {
    base_score <- 0.5 # nocov
  }

  objective <- parsedmodel$general$params$objective

  apply_xgb_objective(f, objective, base_score)
}

# Apply xgboost objective transformation to formula
apply_xgb_objective <- function(f, objective, base_score) {
  if (is.null(objective)) {
    if (base_score != 0) {
      f <- expr_addition(f, base_score)
    }
    cli::cli_warn(
      "If the objective is a custom function,
      please explicitly apply it to the output."
    )
    return(f)
  }

  if (
    objective %in%
      c("reg:squarederror", "reg:squaredlogerror", "binary:logitraw")
  ) {
    if (base_score != 0) {
      f <- expr_addition(f, base_score)
    }
    return(f)
  }

  if (objective %in% c("binary:logistic", "reg:logistic")) {
    return(expr(
      1 - 1 / (1 + exp(!!f + log(!!base_score / (1 - !!base_score))))
    ))
  }

  if (objective %in% c("count:poisson", "reg:tweedie", "reg:gamma")) {
    return(expr(!!base_score * exp(!!f)))
  }

  if (objective %in% c("reg:pseudohubererror", "reg:absoluteerror")) {
    if (base_score != 0) {
      f <- expr_addition(f, base_score)
    }
    return(f)
  }

  if (objective == "binary:hinge") {
    return(expr(as.numeric((!!f + !!base_score) >= 0)))
  }

  cli::cli_abort(
    c(
      "Objective {.val {objective}} is not supported.",
      i = "Supported objectives: {.val binary:hinge}, {.val binary:logistic},
      {.val binary:logitraw}, {.val count:poisson}, {.val reg:absoluteerror},
      {.val reg:gamma}, {.val reg:logistic}, {.val reg:pseudohubererror},
      {.val reg:squarederror}, {.val reg:squaredlogerror}, {.val reg:tweedie}.",
      i = "Multiclass objectives ({.val multi:softmax}, {.val multi:softprob})
      are not supported."
    )
  )
}

# nocov start
# Build condition for xgboost nested generation from path element
# Note: This function is currently not called due to how build_nested_from_flat_paths
# partitions leaves - xgboost's op naming doesn't match the expected convention.
# Kept for potential future use when the partitioning logic is updated.
build_xgb_nested_condition <- function(path_elem) {
  col <- rlang::sym(path_elem$col)
  val <- as.numeric(path_elem$val)
  missing <- path_elem$missing %||% FALSE

  # xgboost uses "less" for left (< threshold) and "more-equal" for right
  if (path_elem$op %in% c("less", "more-equal")) {
    if (missing) {
      expr(!!col < !!val | is.na(!!col))
    } else {
      expr(!!col < !!val)
    }
  } else {
    # This shouldn't happen in normal xgboost trees
    expr(!!col >= !!val)
  }
}
# nocov end

# Extract nested trees from xgboost model
extract_xgb_trees_nested <- function(model) {
  trees_df <- get_xgb_trees_df(model)
  trees_split <- split(trees_df, trees_df$Tree)
  map(trees_split, build_nested_xgb_tree)
}

# Get xgboost trees as data frame
get_xgb_trees_df <- function(model) {
  if (is.null(attr(model, "param"))) {
    # Old xgboost API (< 2.0) - kept for backwards compatibility
    xd <- xgboost::xgb.dump(
      model = model,
      dump_format = "text",
      with_stats = TRUE
    )
    feature_names <- model$feature_names
    trees <- xgboost::xgb.model.dt.tree(text = xd)
  } else {
    feature_names <- xgboost::getinfo(model, "feature_name")
    trees <- xgboost::xgb.model.dt.tree(model = model)
  }
  trees <- as.data.frame(trees)

  # Map feature indices to names if needed
  if (is.null(attr(model, "param"))) {
    feature_names_tbl <- data.frame(
      Feature = as.character(0:(length(feature_names) - 1)),
      feature_name = feature_names,
      stringsAsFactors = FALSE
    )
    trees <- merge(trees, feature_names_tbl, by = "Feature", all.x = TRUE)
  } else {
    trees$feature_name <- ifelse(trees$Feature == "Leaf", NA, trees$Feature)
  }

  # Convert Yes/No/Missing to integer indices
  trees[, c("Yes", "No", "Missing")] <- lapply(
    trees[, c("Yes", "No", "Missing")],
    function(x) {
      dash_pos <- regexpr("-", x, fixed = TRUE)
      as.integer(substring(x, dash_pos + 1L)) + 1L
    }
  )

  trees
}

# Build nested case_when for a single xgboost tree
build_nested_xgb_tree <- function(tree_df) {
  # Pre-extract columns as vectors for fast indexing (avoids slow df[i,] access)
  Feature <- tree_df$Feature
  Gain <- tree_df$Gain %||% tree_df$Quality
  Split <- tree_df$Split
  Yes <- tree_df$Yes
  No <- tree_df$No
  Missing <- tree_df$Missing
  feature_name <- tree_df$feature_name

  build_node <- function(node_idx) {
    # Leaf node
    if (Feature[node_idx] == "Leaf") {
      return(Gain[node_idx])
    }

    # Internal node
    col <- rlang::sym(feature_name[node_idx])
    threshold <- Split[node_idx]
    left_idx <- Yes[node_idx]
    right_idx <- No[node_idx]
    missing_idx <- Missing[node_idx]

    left_subtree <- build_node(left_idx)
    right_subtree <- build_node(right_idx)

    # xgboost: Yes = left (< threshold), No = right (>= threshold)
    # Missing can go either way
    if (missing_idx == left_idx) {
      # Missing goes left: (< threshold OR is.na)
      condition <- expr(!!col < !!threshold | is.na(!!col)) # nocov
    } else {
      # Missing goes right or no missing: < threshold (no NA)
      condition <- expr(!!col < !!threshold)
    }

    expr(case_when(!!condition ~ !!left_subtree, .default = !!right_subtree))
  }

  build_node(1L)
}

# Legacy flat case_when (for v1/v2 parsed model compatibility) ----------------
# These functions generate flat case_when expressions and are preserved for
# backwards compatibility when loading parsed models saved with version < 3.

get_xgb_case <- function(path, prediction) {
  conditions <- map(path, get_xgb_case_fun)
  cl <- combine_path_conditions(conditions)
  expr(!!cl ~ !!prediction)
}

get_xgb_case_fun <- function(.x) {
  if (.x$op == "less") {
    if (.x$missing) {
      i <- expr(
        (!!as.name(.x$col) >= !!as.numeric(.x$val) | is.na(!!as.name(.x$col)))
      )
    } else {
      i <- expr(!!as.name(.x$col) >= !!as.numeric(.x$val))
    }
  } else if (.x$op == "more-equal") {
    if (.x$missing) {
      i <- expr(
        (!!as.name(.x$col) < !!as.numeric(.x$val) | is.na(!!as.name(.x$col)))
      )
    } else {
      i <- expr(!!as.name(.x$col) < !!as.numeric(.x$val))
    }
  }
  i
}

get_xgb_case_tree <- function(tree_no, parsedmodel) {
  map(
    parsedmodel$trees[[tree_no]],
    ~ get_xgb_case(.x$path, .x$prediction)
  )
}

build_fit_formula_xgb <- function(parsedmodel) {
  f <- map(
    seq_len(length(parsedmodel$trees)),
    ~ expr(case_when(!!!get_xgb_case_tree(.x, parsedmodel)))
  )

  # Apply DART weight_drop if present
  f <- apply_dart_weights(f, parsedmodel$general$weight_drop)

  # additive model
  f <- reduce_addition(f)

  base_score <- parsedmodel$general$params$base_score
  if (is.null(base_score)) {
    base_score <- 0.5
  }

  objective <- parsedmodel$general$params$objective

  apply_xgb_objective(f, objective, base_score)
}

# For {orbital} -----------------------------------------------

#' Extract processed xgboost trees
#'
#' For use in orbital package.
#' @param model An xgb.Booster model
#' @keywords internal
#' @export
.extract_xgb_trees <- function(model) {
  if (!inherits(model, "xgb.Booster")) {
    cli::cli_abort(
      "{.arg model} must be {.cls xgb.Booster}, not {.obj_type_friendly {model}}."
    )
  }

  json_params <- get_xgb_json_params(model)
  trees <- extract_xgb_trees_nested(model)
  apply_dart_weights(trees, json_params$weight_drop)
}
