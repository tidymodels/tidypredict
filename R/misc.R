expr_addition <- function(x, y) {
  expr(!!x + !!y)
}

expr_subtraction <- function(x, y) {
  expr(!!x - !!y)
}

expr_multiplication <- function(x, y) {
  expr(!!x * !!y)
}

expr_division <- function(x, y) {
  expr(!!x / !!y)
}

expr_and <- function(x, y) {
  expr(!!x & !!y)
}

reduce_addition <- function(x) {
  reduce(x, expr_addition)
}

reduce_subtraction <- function(x) {
  reduce(x, expr_subtraction)
}

reduce_multiplication <- function(x) {
  reduce(x, expr_multiplication)
}

reduce_and <- function(x) {
  reduce(x, expr_and)
}
