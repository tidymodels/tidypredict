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
