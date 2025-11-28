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

get_xgb_trees <- function(model, filter_trees = TRUE) {
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
    get_xgb_trees_character(xd, feature_names, filter_trees)
  } else {
    feature_names <- xgboost::getinfo(model, "feature_name")
    get_xgb_trees_character(model, feature_names, filter_trees)
  }
}

get_xgb_trees_character <- function(x, feature_names, filter_trees) {
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
  if (filter_trees) {
    trees_rows <- purrr::map_dbl(trees_split, nrow)
    trees_split <- trees_split[trees_rows > 1]
  }

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

  pm$general$version <- 1
  pm$trees <- get_xgb_trees(model)
  as_parsed_model(pm)
}

# Fit model -----------------------------------------------

get_xgb_case <- function(path, prediction) {
  cl <- map(path, get_xgb_case_fun)
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
  } else if (objective %in% c("reg:squarederror", "binary:logitraw")) {
    assigned <- 1
    if (base_score != 0) {
      f <- expr_addition(f, base_score)
    }
  } else if (objective %in% c("binary:logistic", "reg:logistic")) {
    assigned <- 1
    f <- expr(1 - 1 / (1 + exp(!!f + log(!!base_score / (1 - !!base_score)))))
  } else if (objective %in% c("count:poisson")) {
    assigned <- 1
    f <- expr(exp(!!f))
  } else if (objective %in% c("reg:tweedie")) {
    assigned <- 1
    f <- expr(0.5 * exp(!!f)) ## I'm not sure why one has to multiply by 0.5, but it works.
  }
  if (assigned == 0) {
    cli::cli_abort(
      "Only objectives {.val binary:logistic}, {.val reg:squarederror},
      {.val reg:logistic}, {.val binary:logitraw} are supported yet."
    )
  }
  f
}

#' @export
tidypredict_fit.xgb.Booster <- function(model) {
  parsedmodel <- parse_model(model)
  build_fit_formula_xgb(parsedmodel)
}

# For {orbital}
#' Extract processed xgboost trees
#'
#' For use in orbital package.
#' @keywords internal
#' @export
.extract_xgb_trees <- function(model) {
  if (!inherits(model, "xgb.Booster")) {
    cli::cli_abort(
      "{.arg model} must be {.cls xgb.Booster}, not {.obj_type_friendly {x}}."
    )
  }

  params <- model$params
  wosilent <- params[names(params) != "silent"]
  wosilent$silent <- params$silent

  pm <- list()
  pm$general$model <- "xgb.Booster"
  pm$general$type <- "xgb"
  pm$general$niter <- model$niter
  pm$general$params <- wosilent
  pm$general$feature_names <- model$feature_names
  pm$general$nfeatures <- model$nfeatures
  pm$general$version <- 1
  pm$trees <- get_xgb_trees(model, filter_trees = FALSE)

  parsedmodel <- as_parsed_model(pm)
  map(
    seq_len(length(parsedmodel$trees)),
    ~ expr(case_when(!!!get_xgb_case_tree(.x, parsedmodel)))
  )
}
