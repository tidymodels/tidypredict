context("lm")

df <- mtcars
df$cyl <- paste0("cyl", df$cyl)

has_alert <- function(...) tidypredict_test(...)$alert

test_that("Predictions within threshold and parsed model results are equal", {
  expect_false(has_alert(lm(mpg ~ wt, offset = am, data = df)))
  expect_false(has_alert(lm(mpg ~ wt + am + cyl, data = df)))
  expect_false(has_alert(lm(mpg ~ wt + disp * am, data = df)))
  expect_false(has_alert(lm(mpg ~ wt + disp * cyl, data = df)))
  expect_false(has_alert(lm(mpg ~ (wt + disp) * cyl, data = df)))
})

test_that("Intervals return a call", {
  expect_is(tidypredict_interval(lm(mpg ~ wt, data = df)), "call")
  expect_is(tidypredict_interval(lm(mpg ~ (wt + disp) * cyl, data = df)), "call")
  
})
