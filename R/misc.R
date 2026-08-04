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

expr_or <- function(x, y) {
  expr(!!x | !!y)
}

reduce_or <- function(x) {
  reduce(x, expr_or)
}

# Average a set of expressions, as forests do over their trees. `n` is taken
# from the model where it is available, so that the divisor is written the same
# way as the model reports it.
expr_mean <- function(x, n = length(x)) {
  expr_division(reduce_addition(x), n)
}

# Turn per-class scores into class probabilities.
expr_softmax <- function(scores, names = NULL) {
  denom <- reduce_addition(map(scores, ~ expr(exp(!!.x))))
  res <- map(scores, ~ expr(exp(!!.x) / (!!denom)))
  if (!is.null(names)) {
    names(res) <- names
  }
  res
}

# Pick the label whose score is largest.
#
# Expressed as a cascade of `case_when()` conditions rather than a single
# comparison, because a database has no argmax. Each class is compared against
# every class after it, so the first label wins any tie.
build_argmax_case_when <- function(scores, labels) {
  n <- length(scores)
  if (n == 1) {
    return(labels[[1]])
  }

  args <- vector("list", n - 1)
  for (i in seq_len(n - 1)) {
    conditions <- map(
      seq(i + 1, n),
      ~ expr(!!scores[[i]] >= !!scores[[.x]])
    )
    args[[i]] <- expr(
      !!combine_path_conditions(conditions) ~ !!labels[[i]]
    )
  }
  args$.default <- labels[[n]]

  rlang::call2("case_when", !!!args)
}

combine_path_conditions <- function(conditions) {
  n <- length(conditions)
  if (n == 0) {
    return(TRUE)
  }
  if (n == 1) {
    return(conditions[[1]])
  }
  if (n == 2) {
    return(expr_and(conditions[[1]], conditions[[2]]))
  }
  reduce_and(conditions)
}
