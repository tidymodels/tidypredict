context("test-tester")

test_that("Tester returns warning", {
  t <- tidypredict_test(
    lm(mpg ~ wt, offset = am, data = mtcars), 
    threshold = 0)
  expect_true(t$alert)
})
