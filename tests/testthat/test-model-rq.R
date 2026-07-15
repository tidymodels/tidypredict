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
