reduce_addition <- function(x) {
  reduce(x, function(left, right) expr(!!left + !!right))
}

reduce_subtraction <- function(x) {
  reduce(x, function(left, right) expr(!!left - !!right))
}

reduce_multiplication <- function(x) {
  reduce(x, function(left, right) expr(!!left * !!right))
}

reduce_and <- function(x) {
  reduce(x, function(left, right) expr(!!left & !!right))
}
