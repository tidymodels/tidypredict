# Model parser -------------------------------------

get_rf_path <- function(row_id, tree, columns, default_op = TRUE) {
  find <- row_id
  path <- row_id
  for (j in row_id:1) {
    dir <- NULL
    if (tree[j, "left daughter"] == find | tree[j, "right daughter"] == find) {
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
        if (rb["left daughter"] == .x) op <- "less"
        if (rb["right daughter"] == .x) op <- "more-equal"
      } else {
        if (rb["left daughter"] == .x) op <- "less-equal"
        if (rb["right daughter"] == .x) op <- "more"
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

get_rf_tree <- function(tree_no, model) {
  predictions <- model$classes
  term_labels <- names(model$forest$ncat)
  tree <- randomForest::getTree(model, tree_no)
  paths <- seq_len(nrow(tree))[tree[, "status"] == -1]
  purrr::map(
    paths,
    ~ {
      list(
        prediction = predictions[tree[.x, "prediction"]],
        path = get_rf_path(.x, tree, term_labels)
      )
    }
  )
}

get_rf_trees <- function(model) {
  purrr::map(
    seq_len(model$ntree),
    ~ get_rf_tree(.x, model)
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

path_formulas <- function(path) {
  if(length(path) == 1 & path[[1]]$type == "all") {
    rcl <- NULL
  } else {
    cl <- map(
      path,
      ~ {
        i <- NULL
        if(.x$type == "conditional") {
          if (.x$op == "more") i <- expr(!!sym(.x$col) > !!.x$val)
          if (.x$op == "more-equal") i <- expr(!!sym(.x$col) >= !!.x$val)
          if (.x$op == "less") i <- expr(!!sym(.x$col) < !!.x$val)
          if (.x$op == "less-equal") i <- expr(!!sym(.x$col) <= !!.x$val)
        }
        if(.x$type == "set") {
          sets <- reduce(.x$vals, c)
          if (.x$op == "in") i <- expr(!!sym(.x$col) %in% !! sets)
          if (.x$op == "not-in") i <- expr((!!sym(.x$col) %in% !! sets) == FALSE)
        }
        i
      }
    )
    rcl <- reduce(cl, function(x, y) expr(!!x & !!y))
  }
  rcl
}

# Fit model -----------------------------------------------

get_rf_case <- function(path, prediction, calc_mode = "") {
  
  rcl <- path_formulas(path)

  if (length(prediction) > 1) {
    pl <- map(
      prediction,
      ~ {
        if (.x$is_intercept) i <- expr(!!.x$val)
        if (.x$op == "multiply") i <- expr(!!sym(.x$col) * !!.x$val)
        i
      }
    )
    pl <- reduce(pl, function(x, y) expr(!!x + !!y))
  } else {
    pl <- prediction
  }
  f <- NULL
  if (is.null(rcl)) f <- pl
  if (is.null(f) & calc_mode == "ifelse") f <- expr(ifelse(!!rcl, !!pl, 0))
  if (is.null(f)) f <- expr(!!rcl ~ !!pl)
  f
}

get_rf_case_tree <- function(tree_no, parsedmodel) {
  map(
    parsedmodel$trees[[tree_no]],
    ~ get_rf_case(.x$path, .x$prediction, parsedmodel$general$mode)
  )
}

build_fit_formula_rf <- function(parsedmodel) {
  calc_mode <- parsedmodel$general$mode
  if (is.null(calc_mode)) calc_mode <- ""
  divisor <- parsedmodel$general$divisor
  if (is.null(divisor)) divisor <- 1

  f <- NULL

  if (calc_mode == "ifelse") {
    f <- reduce(get_rf_case_tree(1, parsedmodel), function(x, y) expr(!!x + !!y))
    if(divisor > 1) f <- expr(!!f / !!divisor)
  }

  if (is.null(f)) {
    f <- map(
      seq_len(length(parsedmodel$trees)),
      ~ expr(case_when(!!!get_rf_case_tree(.x, parsedmodel)))
    )
  }
  f
}

#' @export
tidypredict_fit.randomForest <- function(model) {
  parsedmodel <- parse_model(model)
  build_fit_formula_rf(parsedmodel)
}
