test_that("expr_addition works", {
  expect_identical(
    expr_addition(quote(hp), 4),
    quote(hp + 4)
  )

  expect_identical(
    expr_addition(quote(hp + hp), 4),
    quote(hp + hp + 4)
  )

  expect_identical(
    expr_addition(quote((hp + hp)), 4),
    quote((hp + hp) + 4)
  )

  expect_identical(
    expr_addition(quote(hp), quote(hp)),
    quote(hp + hp)
  )
})

test_that("expr_subtraction works", {
  expect_identical(
    expr_subtraction(quote(hp), 4),
    quote(hp - 4)
  )

  expect_identical(
    expr_subtraction(quote(hp + hp), 4),
    quote(hp + hp - 4)
  )

  expect_identical(
    expr_subtraction(quote((hp + hp)), 4),
    quote((hp + hp) - 4)
  )

  expect_identical(
    expr_subtraction(quote(hp), quote(hp)),
    quote(hp - hp)
  )
})

test_that("expr_multiplication works", {
  expect_identical(
    expr_multiplication(quote(hp), 4),
    quote(hp * 4)
  )

  expect_identical(
    expr_multiplication(quote(hp + hp), 4),
    quote(`*`(hp + hp, 4))
  )

  expect_identical(
    expr_multiplication(quote((hp + hp)), 4),
    quote((hp + hp) * 4)
  )

  expect_identical(
    expr_multiplication(quote(hp), quote(hp)),
    quote(hp * hp)
  )
})

test_that("expr_division works", {
  expect_identical(
    expr_division(quote(hp), 4),
    quote(hp / 4)
  )

  expect_identical(
    expr_division(quote(hp + hp), 4),
    quote(`/`(hp + hp, 4))
  )

  expect_identical(
    expr_division(quote((hp + hp)), 4),
    quote((hp + hp) / 4)
  )

  expect_identical(
    expr_division(quote(hp), quote(hp)),
    quote(hp / hp)
  )
})

test_that("expr_and works", {
  expect_identical(
    expr_and(quote(hp), 4),
    quote(hp & 4)
  )

  expect_identical(
    expr_and(quote(hp + hp), 4),
    quote(`&`(hp + hp, 4))
  )

  expect_identical(
    expr_and(quote((hp + hp)), 4),
    quote((hp + hp) & 4)
  )

  expect_identical(
    expr_and(quote(hp), quote(hp)),
    quote(hp & hp)
  )
})

test_that("reduce_addition works", {
  expect_identical(
    reduce_addition(list(2, 5, 6)),
    quote(2 + 5 + 6)
  )

  expect_identical(
    reduce_addition(list(2, quote(hp), quote(vp))),
    quote(2 + hp + vp)
  )

  expect_identical(
    reduce_addition(list(2)),
    quote(2)
  )

  expect_identical(
    reduce_addition(list(quote(hp))),
    quote(hp)
  )

  expect_identical(
    reduce_addition(list(quote(vp + vp), quote(vp))),
    quote(vp + vp + vp)
  )

  expect_identical(
    reduce_addition(list(quote(vp + vp), quote((vp + vp)))),
    quote(vp + vp + (vp + vp))
  )
})

test_that("reduce_addition balances only above the threshold (#305)", {
  terms <- as.list(rep(1, addition_balance_at - 1))
  expect_identical(reduce_addition(terms), reduce(terms, expr_addition))

  above <- reduce_addition(as.list(rep(1, addition_balance_at)))
  expect_identical(expr_depth(above), 10L)
  expect_identical(rlang::eval_tidy(above), as.numeric(addition_balance_at))

  # Far past the depth at which R stops evaluating a left fold
  expect_identical(
    rlang::eval_tidy(reduce_addition(as.list(rep(1, 20000)))),
    20000
  )
})

test_that("reduce_subtraction works", {
  expect_identical(
    reduce_subtraction(list(2, 5, 6)),
    quote(2 - 5 - 6)
  )

  expect_identical(
    reduce_subtraction(list(2, quote(hp), quote(vp))),
    quote(2 - hp - vp)
  )

  expect_identical(
    reduce_subtraction(list(2)),
    quote(2)
  )

  expect_identical(
    reduce_subtraction(list(quote(hp))),
    quote(hp)
  )

  expect_identical(
    reduce_subtraction(list(quote(vp + vp), quote(vp))),
    quote(vp + vp - vp)
  )

  expect_identical(
    reduce_subtraction(list(quote(vp + vp), quote((vp + vp)))),
    quote(vp + vp - (vp + vp))
  )
})

test_that("reduce_multiplication works", {
  expect_identical(
    reduce_multiplication(list(2, 5, 6)),
    quote(2 * 5 * 6)
  )

  expect_identical(
    reduce_multiplication(list(2, quote(hp), quote(vp))),
    quote(2 * hp * vp)
  )

  expect_identical(
    reduce_multiplication(list(2)),
    quote(2)
  )

  expect_identical(
    reduce_multiplication(list(quote(hp))),
    quote(hp)
  )

  expect_identical(
    reduce_multiplication(list(quote((vp + vp)), quote(vp))),
    quote((vp + vp) * vp)
  )

  expect_identical(
    reduce_multiplication(list(quote((vp + vp)), quote((vp + vp)))),
    quote((vp + vp) * (vp + vp))
  )
})

test_that("reduce_and works", {
  expect_identical(
    reduce_and(list(2, 5, 6)),
    quote(2 & 5 & 6)
  )

  expect_identical(
    reduce_and(list(2, quote(hp), quote(vp))),
    quote(2 & hp & vp)
  )

  expect_identical(
    reduce_and(list(2)),
    quote(2)
  )

  expect_identical(
    reduce_and(list(quote(hp))),
    quote(hp)
  )

  expect_identical(
    reduce_and(list(quote((vp + vp)), quote(vp))),
    quote((vp + vp) & vp)
  )

  expect_identical(
    reduce_and(list(quote((vp + vp)), quote((vp + vp)))),
    quote((vp + vp) & (vp + vp))
  )
})

test_that("expr_softmax survives scores that would overflow (#299)", {
  # `exp(800)` is `Inf`, so the textbook `exp(s_k) / sum(exp(s_j))` returns
  # `NaN` from `Inf / Inf` for every class.
  scores <- list(quote(a), quote(b), quote(c))
  probs <- expr_softmax(scores, c("x", "y", "z"))

  eval_at <- function(a, b, c) {
    vapply(
      probs,
      \(p) rlang::eval_tidy(p, list(a = a, b = b, c = c)),
      numeric(1)
    )
  }

  expect_equal(eval_at(800, 0, -800), c(x = 1, y = 0, z = 0))
  expect_equal(eval_at(-800, 800, 0), c(x = 0, y = 1, z = 0))

  # Ordinary scores still give the ordinary answer
  expect_equal(
    eval_at(1, 2, 3),
    c(x = exp(1), y = exp(2), z = exp(3)) / sum(exp(1:3))
  )

  # A constant added to every score leaves the result unchanged, which is the
  # property the old form lost
  expect_equal(eval_at(1, 2, 3), eval_at(1001, 1002, 1003))
})
