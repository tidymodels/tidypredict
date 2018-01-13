context("lm")

df <- mtcars %>%
  mutate(cyl = paste0("cyl", cyl))

has_alert <- function(model){
  tidypredict_test(
    model,
    include_intervals = TRUE
  )$alert
}

test_that("Individual prediction difference is never above 1e-df", {
  expect_false(has_alert( lm( mpg ~ wt, offset = am,   data = df) )) 
  expect_false(has_alert( lm( mpg ~ wt + am + cyl,     data = df) ))
  expect_false(has_alert( lm( mpg ~ wt + disp * am,    data = df) ))
  expect_false(has_alert( lm( mpg ~ wt + disp * cyl,   data = df) ))
  expect_false(has_alert( lm( mpg ~ (wt + disp) * cyl, data = df) ))
})
