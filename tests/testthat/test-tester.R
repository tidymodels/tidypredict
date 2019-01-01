context("test-tester")

test_that("Tester returns warning", {
  t <- tidypredict_test(
    lm(mpg ~ ., data = mtcars),
    threshold = 0)
  expect_false(t$alert)
})

test_that("Intervals returns list",{
  expect_is(
    tidypredict_test(lm(mpg ~ ., data = mtcars), include_intervals = TRUE),
    "tidypredict_test"
  )
})
