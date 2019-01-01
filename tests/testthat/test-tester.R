context("test-tester")

test_that("Tester returns warning", {
  t <- tidypredict_test(
    lm(mpg ~ ., data = mtcars),
    threshold = 0)
  expect_false(t$alert)
})
