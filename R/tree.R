#' Generate trees
#'
#' Each tree is generated as a flat tree with each node being a seperate part of
#' the case when.
#' This means that the following tree:
#'
#'             +-----+
#'        +----|x > 0|----+
#'        |    +-----+    |
#'        v               v
#'    +------+        +--------+
#' +--|y < 20|--+  +--|z <= 10 |--+
#' |  +------+  |  |  +--------+  |
#' v            v  v              v
#' a            b  c              d
#'
#' will be turned into the following `case_when()` statement.
#'
#' ```r
#' case_when(
#'   x >  0 & y <  20 ~ "a",
#'   x >  0 & y >= 20 ~ "b",
#'   x <= 0 & z <= 10 ~ "c",
#'   x <= 0 & z >  10 ~ "d"
#' )
#' ```
#'
#' instead of a nested `case_when()`s` like this
#'
#' ```r
#' case_when(
#'   x >  0 ~ case_when(
#'              y <  20 ~ "a",
#'              y >= 10 ~ "b"
#'            ),
#'   x <= 0 ~ case_when(
#'              z <= 10 ~ "c",
#'              z >  10 ~ "d"
#'            )
#' )
#' ```
#'
#' The functions in this file generates these tree.
#' `generate_case_when_tree()` generates a single tree with
#' `generate_case_when_trees()` being a convinience wrapper for multiple trees.
#'
#' `generate_tree_node()` generates the expressions for each a single ndoe in
#' the tree, where `generate_tree_nodes()` is a convinience wrapper for
#' calculating all notes.
#'
#' @keywords internal
generate_case_when_trees <- function(parsedmodel, default = TRUE) {
  map(
    parsedmodel$trees,
    generate_case_when_tree,
    mode = parsedmodel$general$mode,
    default = default
  )
}

generate_case_when_tree <- function(tree, mode, default = TRUE) {
  nodes <- generate_tree_nodes(tree, mode)

  if (default) {
    default <- nodes[[length(nodes)]]
    default <- rlang::f_rhs(default)
    nodes[[length(nodes)]] <- NULL
    nodes <- c(nodes, .default = default)
  }

  expr(case_when(!!!nodes))
}

generate_tree_nodes <- function(tree, mode) {
  map(tree, generate_tree_node, mode)
}

#' Construct a single node of a tree
#'
#' @param node a list with named elements `path` and `prediction`. See details
#' for more.
#' @param calc_mode character, takes values `""` and `"calc_mode"`.
#'
#' The `node` list should contain the two lists `path` and `prediction`.
#'
#' The `path` element has the following structure:
#'
#' This list can contain 0 or more elemements. The elements but each be of the
#' following format:
#'
#' - `type` character, must be `"conditional"`, `"set"`, or `"all"`.
#' - `op` character.
#'   if `type == "conditional"` must be `"more"`, `"more-equal"`, `"less"`, or
#'   `"less-equal"`.
#'   if `type == "set"` must be `"in"` on `not-in`.
#' - `col` character.
#' - `val` if `type == "conditional"` and `vals` if `type == "set"`.
#'   Can be character or numeric.
#'
#' The `prediction` list has the following structure:
#'
#' It can either be a singular value or a list.
#' If it is a list it will have the following 4 named elements `col`, `val`,
#' `op`, and `is_intercept`.
#'
#' - `col` character, name of column
#' - `val` val, numeric of character
#' - `op` character, known values are `"none"` and `"multiply"`. `"none"` is
#'   used then `is_intercept == 1`.
#' - `is_intercept`integer, takes values `0` and `1`.`
#'
#'  @keywords internal
generate_tree_node <- function(node, calc_mode = "") {
  path <- node$path
  prediction <- node$prediction
  rcl <- path_formulas(path)

  if (length(prediction) > 1) {
    pl <- map(
      prediction,
      ~ {
        if (.x$is_intercept) {
          if (.x$val == 0) {
            return(NULL)
          }
          return(expr(!!.x$val))
        } else if (.x$op == "multiply") {
          if (.x$val == 0) {
            return(NULL)
          }

          if (.x$val == 1) {
            return(expr(!!as.name(.x$col)))
          }

          return(expr_multiplication(as.name(.x$col), .x$val))
        }
      }
    )
    pl <- purrr::discard(pl, is.null)
    pl <- reduce_addition(pl)
  } else {
    if (is.list(prediction) && prediction[[1]]$is_intercept) {
      prediction <- prediction[[1]]$val
    }
    pl <- prediction
  }

  if (isTRUE(rcl)) {
    return(pl)
  }

  if (calc_mode == "ifelse") {
    return(expr(ifelse(!!rcl, !!pl, 0)))
  }

  expr(!!rcl ~ !!pl)
}

#' Turn a path object into a combined expression
#'
#' @param path a list of lists.
#'
#' This list can contain 0 or more elemements. The elements but each be of the
#' following format:
#'
#' - `type` character, must be `"conditional"`, `"set"`, or `"all"`.
#' - `op` character.
#'   if `type == "conditional"` must be `"more"`, `"more-equal"`, `"less"`, or
#'   `"less-equal"`.
#'   if `type == "set"` must be `"in"` on `not-in`.
#' - `col` character.
#' - `val` if `type == "conditional"` and `vals` if `type == "set"`.
#'   Can be character or numeric.
#'  @keywords internal
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
      i <- expr(!!as.name(x$col) > !!x$val)
    } else if (x$op == "more-equal") {
      i <- expr(!!as.name(x$col) >= !!x$val)
    } else if (x$op == "less") {
      i <- expr(!!as.name(x$col) < !!x$val)
    } else if (x$op == "less-equal") {
      i <- expr(!!as.name(x$col) <= !!x$val)
    } else {
      cli::cli_abort(
        "{.field op} has unsupported value of {.value {x$op}}.",
        .internal = TRUE
      )
    }
  } else if (x$type == "set") {
    sets <- reduce(x$vals, c)
    if (x$op == "in") {
      i <- expr(!!as.name(x$col) %in% !!sets)
    } else if (x$op == "not-in") {
      i <- expr((!!as.name(x$col) %in% !!sets) == FALSE)
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
