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
  # To deal with new agboost version
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

  pm$general$version <- 1
  pm$trees <- get_xgb_trees(model)
  as_parsed_model(pm)
}

# Fit model -----------------------------------------------

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
  assigned <- 0
  if (is.null(objective)) {
    assigned <- 1
    if (base_score != 0) {
      f <- expr_addition(f, base_score)
    }
    cli::cli_warn(
      "If the objective is a custom function, 
      please explicitly apply it to the output."
    )
  } else if (
    objective %in%
      c(
        "reg:squarederror",
        "reg:squaredlogerror",
        "binary:logitraw"
      )
  ) {
    assigned <- 1
    if (base_score != 0) {
      f <- expr_addition(f, base_score)
    }
  } else if (objective %in% c("binary:logistic", "reg:logistic")) {
    assigned <- 1
    f <- expr(1 - 1 / (1 + exp(!!f + log(!!base_score / (1 - !!base_score)))))
  } else if (objective %in% c("count:poisson", "reg:tweedie", "reg:gamma")) {
    assigned <- 1
    f <- expr(!!base_score * exp(!!f))
  } else if (objective %in% c("reg:pseudohubererror", "reg:absoluteerror")) {
    assigned <- 1
    if (base_score != 0) {
      f <- expr_addition(f, base_score)
    }
  } else if (objective == "binary:hinge") {
    assigned <- 1
    # binary:hinge returns 0 or 1 based on margin sign
    f <- expr(as.numeric((!!f + !!base_score) >= 0))
  }
  if (assigned == 0) {
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
  f
}

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

  json <- jsonlite::fromJSON(tmp_file)

  base_score <- json$learner$learner_model_param$base_score
  base_score <- gsub("\\[", "", base_score)
  base_score <- gsub("\\]", "", base_score)
  base_score <- strsplit(base_score, ",")[[1]]
  base_score <- as.numeric(base_score)

  booster_name <- json$learner$gradient_booster$name
  weight_drop <- json$learner$gradient_booster$weight_drop

  list(
    base_score = base_score,
    booster_name = booster_name,
    weight_drop = weight_drop
  )
}

get_base_score <- function(model) {
  get_xgb_json_params(model)$base_score
}

#' @export
tidypredict_fit.xgb.Booster <- function(model, nested = FALSE, ...) {
  if (nested) {
    build_fit_formula_xgb_nested(model)
  } else {
    parsedmodel <- parse_model(model)
    build_fit_formula_xgb(parsedmodel)
  }
}

# Build nested xgboost formula
build_fit_formula_xgb_nested <- function(model) {
  trees_nested <- extract_xgb_trees_nested(model)

  # Apply DART weight_drop if present
  json_params <- get_xgb_json_params(model)
  trees_nested <- apply_dart_weights(trees_nested, json_params$weight_drop)

  # Additive model
  f <- reduce_addition(trees_nested)

  base_score <- json_params$base_score
  if (is.null(base_score)) {
    base_score <- 0.5
  }

  params <- attr(model, "param") %||% model$params
  objective <- params$objective

  assigned <- 0
  if (is.null(objective)) {
    assigned <- 1
    if (base_score != 0) {
      f <- expr_addition(f, base_score)
    }
    cli::cli_warn(
      "If the objective is a custom function,
      please explicitly apply it to the output."
    )
  } else if (
    objective %in%
      c("reg:squarederror", "reg:squaredlogerror", "binary:logitraw")
  ) {
    assigned <- 1
    if (base_score != 0) {
      f <- expr_addition(f, base_score)
    }
  } else if (objective %in% c("binary:logistic", "reg:logistic")) {
    assigned <- 1
    f <- expr(1 - 1 / (1 + exp(!!f + log(!!base_score / (1 - !!base_score)))))
  } else if (objective %in% c("count:poisson", "reg:tweedie", "reg:gamma")) {
    assigned <- 1
    f <- expr(!!base_score * exp(!!f))
  } else if (objective %in% c("reg:pseudohubererror", "reg:absoluteerror")) {
    assigned <- 1
    if (base_score != 0) {
      f <- expr_addition(f, base_score)
    }
  } else if (objective == "binary:hinge") {
    assigned <- 1
    f <- expr(as.numeric((!!f + !!base_score) >= 0))
  }
  if (assigned == 0) {
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
  f
}

# Extract nested trees from xgboost model
extract_xgb_trees_nested <- function(model) {
  trees_df <- get_xgb_trees_df(model)
  trees_split <- split(trees_df, trees_df$Tree)
  map(trees_split, build_nested_xgb_tree)
}

# Get xgboost trees as data frame
get_xgb_trees_df <- function(model) {
  if (is.null(attr(model, "param"))) {
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
    function(x) as.integer(sub("^.*-", "", x)) + 1L
  )

  trees
}

# Build nested case_when for a single xgboost tree
build_nested_xgb_tree <- function(tree_df) {
  # tree_df has Node (0-indexed), Feature, Split, Yes, No, Missing, Quality/Gain
  build_nested_xgb_node(1L, tree_df)
}

build_nested_xgb_node <- function(node_idx, tree_df) {
  row <- tree_df[node_idx, ]

  # Leaf node
  if (row$Feature == "Leaf") {
    return(row$Gain %||% row$Quality)
  }

  # Internal node
  col <- rlang::sym(row$feature_name)
  threshold <- row$Split
  left_idx <- row$Yes
  right_idx <- row$No
  missing_idx <- row$Missing

  left_subtree <- build_nested_xgb_node(left_idx, tree_df)
  right_subtree <- build_nested_xgb_node(right_idx, tree_df)

  # xgboost: Yes = left (< threshold), No = right (>= threshold)
  # Missing can go either way
  if (missing_idx == left_idx) {
    # Missing goes left: (< threshold OR is.na)
    condition <- expr(!!col < !!threshold | is.na(!!col))
  } else if (missing_idx == right_idx) {
    # Missing goes right: < threshold (no NA)
    condition <- expr(!!col < !!threshold)
  } else {
    # No missing handling
    condition <- expr(!!col < !!threshold)
  }

  expr(case_when(!!condition ~ !!left_subtree, .default = !!right_subtree))
}

# For {orbital}
#' Extract processed xgboost trees
#'
#' For use in orbital package.
#' @param model An xgb.Booster model
#' @param nested Logical, whether to use nested case_when (default FALSE)
#' @keywords internal
#' @export
.extract_xgb_trees <- function(model, nested = FALSE) {
  if (!inherits(model, "xgb.Booster")) {
    cli::cli_abort(
      "{.arg model} must be {.cls xgb.Booster}, not {.obj_type_friendly {model}}."
    )
  }

  json_params <- get_xgb_json_params(model)

  if (nested) {
    trees <- extract_xgb_trees_nested(model)
  } else {
    parsedmodel <- parse_model(model)
    trees <- map(
      seq_len(length(parsedmodel$trees)),
      ~ expr(case_when(!!!get_xgb_case_tree(.x, parsedmodel)))
    )
  }

  apply_dart_weights(trees, json_params$weight_drop)
}
