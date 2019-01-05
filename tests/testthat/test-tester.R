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

test_that("Alert is returned", {
  expect_true(
    tidypredict_test(
      lm(mpg ~ wt, offset = am, data = mtcars), 
      include_intervals = TRUE, 
      threshold = 0
    )$alert
  )
})

test_that("Error is returned for tree based models", {
  expect_error(
    tidypredict_test(randomForest::randomForest(Species ~., data = iris)),
    "tidypredict_test does not support"
  )
  expect_error(
    tidypredict_test(ranger::ranger(Species ~., data = iris)),
    "tidypredict_test does not support"
  )  
})

