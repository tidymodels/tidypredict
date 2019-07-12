context("test-tester")

test_that("Tester returns warning", {
  t <- tidypredict_test(
    lm(mpg ~ wt, data = mtcars),
    threshold = 1
  )
  expect_false(t$alert)
})

test_that("Intervals returns list", {
  expect_is(
    tidypredict_test(lm(mpg ~ ., data = mtcars), include_intervals = TRUE),
    "tidypredict_test"
  )
})

test_that("Error is returned for tree based models", {
  expect_error(
    tidypredict_test(randomForest::randomForest(Species ~ ., data = iris), df = iris),
    "tidypredict_test does not support"
  )
  expect_error(
    tidypredict_test(ranger::ranger(Species ~ ., data = iris), df = iris),
    "tidypredict_test does not support"
  )
})

test_that("Expect error message", {
  expect_error(
    tidypredict_test(earth::earth(am ~ ., data = mtcars)),
    "Test data is missing"
  )
})
