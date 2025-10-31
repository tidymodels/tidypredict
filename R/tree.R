get_rf_case_tree <- function(tree_no, parsedmodel) {
  map(
    parsedmodel$trees[[tree_no]],
    ~ get_rf_case(.x$path, .x$prediction, parsedmodel$general$mode)
  )
}


get_rf_case <- function(path, prediction, calc_mode = "") {
  rcl <- path_formulas(path)

  if (length(prediction) > 1) {
    pl <- map(
      prediction,
      ~ {
        if (.x$is_intercept) {
          i <- expr(!!.x$val)
        }
        if (.x$op == "multiply") {
          i <- expr(!!sym(.x$col) * !!.x$val)
        }
        i
      }
    )
    pl <- reduce(pl, function(x, y) expr(!!x + !!y))
  } else {
    if (is.list(prediction) && prediction[[1]]$is_intercept) {
      prediction <- prediction[[1]]$val
    }
    pl <- prediction
  }
  f <- NULL
  if (is.null(rcl) || isTRUE(rcl)) {
    f <- pl
  }
  if (is.null(f) & calc_mode == "ifelse") {
    f <- expr(ifelse(!!rcl, !!pl, 0))
  }
  if (is.null(f)) {
    f <- expr(!!rcl ~ !!pl)
  }
  f
}

path_formulas <- function(path) {
  if (length(path) == 0) {
    return(TRUE)
  }

  if (length(path) == 1 & path[[1]]$type == "all") {
    rcl <- NULL
  } else {
    cl <- map(
      path,
      ~ {
        i <- NULL
        if (.x$type == "conditional") {
          if (.x$op == "more") {
            i <- expr(!!sym(.x$col) > !!.x$val)
          }
          if (.x$op == "more-equal") {
            i <- expr(!!sym(.x$col) >= !!.x$val)
          }
          if (.x$op == "less") {
            i <- expr(!!sym(.x$col) < !!.x$val)
          }
          if (.x$op == "less-equal") i <- expr(!!sym(.x$col) <= !!.x$val)
        }
        if (.x$type == "set") {
          sets <- reduce(.x$vals, c)
          if (.x$op == "in") {
            i <- expr(!!sym(.x$col) %in% !!sets)
          }
          if (.x$op == "not-in") i <- expr((!!sym(.x$col) %in% !!sets) == FALSE)
        }
        i
      }
    )
    rcl <- reduce(cl, function(x, y) expr(!!x & !!y))
  }
  rcl
}
