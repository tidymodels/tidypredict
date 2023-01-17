# Model parser -------------------------------------

get_xgb_path <- function(row_id, tree) {
  find <- row_id
  path <- row_id
  for (j in row_id:1) {
    dir <- NULL
    if (tree[j, "Yes"] %in% find | tree[j, "No"] %in% find) {
      find <- j
      path <- c(path, j)
    }
  }
  purrr::map2(
    path[1:length(path) - 1],
    path[2:length(path)],
    ~ {
      rb <- tree[.y, ]
      if (rb["Yes"] %in% .x) {
        op <- "more-equal"
        missing <- rb["Missing"] %in% rb["Yes"]
      }
      if (rb["No"] %in% .x) {
        op <- "less"
        missing <- rb["Missing"] %in% rb["No"]
      }
      list(
        type = "conditional",
        col = rb$feature_name,
        val = rb$Split,
        op = op,
        missing = missing
      )
    }
  )
}

get_xgb_tree <- function(tree) {
  paths <- seq_len(nrow(tree))[tree[, "Feature"] == "Leaf"]
  x <- map(
    paths,
    ~ {
      list(
        prediction = tree[.x, "Quality", drop = TRUE],
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
  feature_names <- model$feature_names
  get_xgb_trees_character(xd, feature_names)
}

get_xgb_trees_character <- function(xd, feature_names) {
  feature_names_tbl <- data.frame(
    Feature = as.character(0:(length(feature_names) - 1)),
    feature_name = feature_names,
    stringsAsFactors = FALSE
  )
  trees <- xgboost::xgb.model.dt.tree(text = xd)
  trees <- as.data.frame(trees)
  trees$original_order <- 1:nrow(trees)
  trees <- merge(trees, feature_names_tbl, by = "Feature", all.x = TRUE)
  trees <- trees[order(trees$original_order), !names(trees) %in% "original_order"]
  trees[, c("Yes", "No", "Missing")] <-
    lapply(trees[, c("Yes", "No", "Missing")], function(x) sub("^.*-", "", x))
  trees[, c("Yes", "No", "Missing")] <-
    lapply(trees[, c("Yes", "No", "Missing")], function(x) as.integer(x) + 1)

  trees_split <- split(trees, trees$Tree)
  trees_rows <- purrr::map_dbl(trees_split, nrow)
  trees_filtered <- trees_split[trees_rows > 1]
  
  purrr::map(trees_filtered, get_xgb_tree)
}

#' @export
parse_model.xgb.Booster <- function(model) {
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
  pm$trees <- get_xgb_trees(model)
  as_parsed_model(pm)
}

# Fit model -----------------------------------------------

get_xgb_case <- function(path, prediction) {
  cl <- map(
    path,
    ~ {
      if (.x$op == "less" & .x$missing) {
        i <- expr((!!sym(.x$col) >= !!as.numeric(.x$val) | is.na(!!sym(.x$col))))
      }
      if (.x$op == "more-equal" & .x$missing) {
        i <- expr((!!sym(.x$col) < !!as.numeric(.x$val) | is.na(!!sym(.x$col))))
      }
      if (.x$op == "less" & !.x$missing) {
        i <- expr(!!sym(.x$col) >= !!as.numeric(.x$val))
      }
      if (.x$op == "more-equal" & !.x$missing) {
        i <- expr(!!sym(.x$col) < !!as.numeric(.x$val))
      }
      i
    }
  )
  cl <- if (length(cl) > 0) {
    reduce(cl, function(x, y) expr(!!x & !!y))
  } else {
    TRUE
  }
  expr(!!cl ~ !!prediction)
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
  f <- purrr::reduce(f, ~ expr(!!.x + !!.y), .init = expr(0))

  base_score <- parsedmodel$general$params$base_score
  if (is.null(base_score)) base_score <- 0.5

  objective <- parsedmodel$general$params$objective
  assigned <- 0
  if (is.null(objective)) {
    assigned <- 1
    f <- expr(!!f + !!base_score)
    warning(
      paste(
        "If the objective is a custom function, please",
        "explicitly apply it to the output."
      )
    )
  } else if (objective %in% c("reg:squarederror", "binary:logitraw")) {
    assigned <- 1
    f <- expr(!!f + !!base_score)
  } else if (objective %in% c("binary:logistic", "reg:logistic")) {
    assigned <- 1
    f <- expr(1 - 1 / (1 + exp(!!f + log(!!base_score / (1 - !!base_score)))))
  }
  if (assigned == 0) {
    stop(
      paste0(
        "Only objectives 'binary:logistic', 'reg:squarederror',",
        "'reg:logistic', 'binary:logitraw' are supported yet."
      )
    )
  }
  f
}

#' @export
tidypredict_fit.xgb.Booster <- function(model) {
  parsedmodel <- parse_model(model)
  build_fit_formula_xgb(parsedmodel)
}
