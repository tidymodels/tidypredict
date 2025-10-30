skip_if_not_installed("randomForest")

test_that("Tester returns warning", {
  t <- tidypredict_test(
    lm(mpg ~ wt, data = mtcars),
    threshold = 1
  )
  expect_false(t$alert)
})

test_that("Intervals returns list", {
  expect_s3_class(
    tidypredict_test(lm(mpg ~ ., data = mtcars), include_intervals = TRUE),
    "tidypredict_test"
  )
})
