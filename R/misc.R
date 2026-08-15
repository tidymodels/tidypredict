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

# Number of terms above which `reduce_addition()` sums in a balanced shape
# rather than from the left.
#
# Measured rather than guessed. A left fold of `n` terms nests the `+` calls `n`
# deep, and at the default `options(expressions = 5000)` the deepest that still
# evaluates is about 4990 terms, falling to about 4890 when the call arrives 100
# frames down, as it does through dplyr. Both numbers move with `expressions`
# and with how deep the caller already is, so the threshold wants a wide margin
# rather than a close one: 1000 sits a fivefold margin under the wall and still
# well above every ensemble at its package default, the largest of which sums
# 500 terms.
addition_balance_at <- 1000L

# Sum a set of expressions.
#
# The left fold is preferred: it is what the modelling packages do themselves,
# so the result usually agrees with `predict()` bit for bit, and it prints as
# the flat `a + b + c` rather than a thicket of parentheses. Its nesting depth
# is the number of terms, though, and past a few thousand R gives up with
# "evaluation nested too deeply". Only an ensemble gets near that, and only a
# large one: a `bart()` fit sums `n_draws * ntree` leaf values, which is 10,000
# terms at dbarts' default `ntree`. Those are summed in a balanced shape
# instead, which nests `log2(length(x))` deep and so always evaluates.
reduce_addition <- function(x) {
  if (length(x) < addition_balance_at) {
    return(reduce(x, expr_addition))
  }
  reduce_addition_balanced(x)
}

reduce_addition_balanced <- function(x) {
  n <- length(x)
  if (n == 0) {
    return(NULL)
  }
  if (n == 1) {
    return(x[[1]])
  }
  mid <- n %/% 2
  expr_addition(
    reduce_addition_balanced(x[seq_len(mid)]),
    reduce_addition_balanced(x[seq.int(mid + 1, n)])
  )
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

# Return `NA` for any row with a missing predictor.
#
# Some backends decline to predict from an incomplete row: `randomForest` and
# `kernlab::ksvm` return nothing for it, and `aorsf` refuses outright. Their
# trees still contain a complete set of branches, so the generated expression
# would otherwise route the row on whichever comparisons happen not to involve
# the missing column and return a confident value the model itself would never
# produce.
#
# The guard is deliberately blanket rather than per split: `randomForest`
# returns `NA` whenever *any* predictor is missing, even one the row's path
# never consults, so testing every column is what reproduces it.
expr_na_if_incomplete <- function(f, cols, missing = NA_real_) {
  cols <- unique(cols)
  if (length(cols) == 0) {
    return(f)
  }
  any_missing <- reduce_or(map(cols, \(col) expr(is.na(!!rlang::sym(col)))))
  expr(ifelse(!!any_missing, !!missing, !!f))
}

# Average a set of expressions, as forests do over their trees. `n` is taken
# from the model where it is available, so that the divisor is written the same
# way as the model reports it.
expr_mean <- function(x, n = length(x)) {
  expr_division(reduce_addition(x), n)
}

# The inverse logit, mapping a linear predictor onto a probability.
#
# Written as `1 / (1 + exp(-f))` rather than the algebraically equal
# `1 - 1 / (1 + exp(f))`, because the latter rounds to exactly 0 once `exp(f)`
# reaches 1 in double precision, losing every small probability.
expr_logistic <- function(f) {
  expr(1 / (1 + exp(-(!!f))))
}

# Turn per-class scores into class probabilities.
#
# Written as `1 / sum_j exp(s_j - s_k)` rather than the textbook
# `exp(s_k) / sum_j exp(s_j)`, which overflows to `Inf / Inf` and returns `NaN`
# for any score above about 710. Subtracting `s_k` inside each `exp()` is what
# the reference implementations achieve by subtracting the row maximum, but it
# needs no `pmax()` and leaves the expression the same size. The `j == k` term
# is `exp(0)`, written as `1`, so the denominator is never below 1 and cannot
# underflow to zero either.
expr_softmax <- function(scores, names = NULL) {
  res <- map(seq_along(scores), function(k) {
    terms <- map(seq_along(scores), function(j) {
      if (j == k) {
        return(1)
      }
      expr(exp(!!scores[[j]] - !!scores[[k]]))
    })
    expr(1 / (!!reduce_addition(terms)))
  })
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
