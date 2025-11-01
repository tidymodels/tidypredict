generate_case_when_trees <- function(parsedmodel) {
  map(
    parsedmodel$trees,
    generate_case_when_tree,
    mode = parsedmodel$general$mode
  )
}

generate_case_when_tree <- function(tree, mode) {
  expr(case_when(!!!generate_cases(tree, mode)))
}

generate_cases <- function(tree, mode) {
  map(
    tree,
    ~ generate_case(.x$path, .x$prediction, mode)
  )
}

generate_case <- function(path, prediction, calc_mode = "") {
  rcl <- path_formulas(path)

  if (length(prediction) > 1) {
    pl <- map(
      prediction,
      ~ {
        if (.x$is_intercept) {
          i <- expr(!!.x$val)
        }
        if (.x$op == "multiply") {
          i <- expr_multiplication(sym(.x$col), .x$val)
        }
        i
      }
    )
    pl <- reduce_addition(pl)
  } else {
    if (is.list(prediction) && prediction[[1]]$is_intercept) {
      prediction <- prediction[[1]]$val
    }
    pl <- prediction
  }
  f <- NULL
  if (isTRUE(rcl)) {
    f <- pl
  }
  if (is.null(f) && calc_mode == "ifelse") {
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

  if (length(path) == 1 && path[[1]]$type == "all") {
    return(TRUE)
  }

  cl <- map(path, path_formula)
  res <- reduce_and(cl)
  res
}

#' Turn a path object into an expression
#'
#' @param x a list.
#'
#' The input of this function is a list with 4 values.
#'
#' - `type` character, must be `"conditional"` or `"set"`.
#' - `op` character.
#'   if `type == "conditional"` must be `"more"`, `"more-equal"`, `"less"`, or
#'   `"less-equal"`.
#'   if `type == "set"` must be `"in"` on `not-in`.
#' - `col` character.
#' - `val` if `type == "conditional"` and `vals` if `type == "set"`.
#'   Can be character or numeric.
#'  @keywords internal
path_formula <- function(x) {
  if (x$type == "conditional") {
    if (x$op == "more") {
      i <- expr(!!sym(x$col) > !!x$val)
    } else if (x$op == "more-equal") {
      i <- expr(!!sym(x$col) >= !!x$val)
    } else if (x$op == "less") {
      i <- expr(!!sym(x$col) < !!x$val)
    } else if (x$op == "less-equal") {
      i <- expr(!!sym(x$col) <= !!x$val)
    } else {
      cli::cli_abort(
        "{.field op} has unsupported value of {.value {x$op}}.",
        .internal = TRUE
      )
    }
  } else if (x$type == "set") {
    sets <- reduce(x$vals, c)
    if (x$op == "in") {
      i <- expr(!!sym(x$col) %in% !!sets)
    } else if (x$op == "not-in") {
      i <- expr((!!sym(x$col) %in% !!sets) == FALSE)
    } else {
      cli::cli_abort(
        "{.field op} has unsupported value of {.value {x$op}}.",
        .internal = TRUE
      )
    }
  } else {
    cli::cli_abort(
      "{.field type} has unsupported value of {.value {x$type}}.",
      .internal = TRUE
    )
  }
  i
}
