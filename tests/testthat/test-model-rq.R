test_that("returns the right output", {
  skip_if_not_installed("quantreg")

  model <- quantreg::rq(mpg ~ wt + cyl, data = mtcars)

  tf <- tidypredict_fit(model)
  pm <- parse_model(model)

  expect_type(tf, "language")

  expect_s3_class(pm, "list")
  expect_equal(length(pm), 2)
  expect_equal(pm$general$model, "rq")
  expect_equal(pm$general$version, 2)

  expect_snapshot(
    rlang::expr_text(tf)
  )
})

test_that("formulas produces correct predictions", {
  skip_if_not_installed("quantreg")

  mtcars$cyl <- paste0("cyl", mtcars$cyl)
  expect_snapshot(
    tidypredict_test(
      quantreg::rq(mpg ~ wt + cyl + disp, data = mtcars),
      mtcars
    )
  )
})

test_that("works with non-default tau, method, and weights", {
  skip_if_not_installed("quantreg")

  expect_type(
    tidypredict_fit(quantreg::rq(mpg ~ wt + cyl, data = mtcars, tau = 0.9)),
    "language"
  )
  expect_type(
    tidypredict_fit(quantreg::rq(mpg ~ wt + cyl, data = mtcars, method = "fn")),
    "language"
  )
  w <- rep(c(1, 2), length.out = nrow(mtcars))
  expect_type(
    tidypredict_fit(quantreg::rq(mpg ~ wt + cyl, data = mtcars, weights = w)),
    "language"
  )
})

test_that("returns one expression per quantile for multiple quantiles", {
  skip_if_not_installed("quantreg")

  model <- quantreg::rq(mpg ~ wt + cyl, data = mtcars, tau = c(0.25, 0.5, 0.75))
  tf <- tidypredict_fit(model)

  expect_type(tf, "list")
  expect_named(tf, c("0.25", "0.50", "0.75"))
  expect_true(all(vapply(tf, is.language, logical(1))))

  expect_snapshot(
    lapply(tf, rlang::expr_text)
  )
})
