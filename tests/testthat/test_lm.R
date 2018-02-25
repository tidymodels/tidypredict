context("lm")

df <- mtcars %>%
  mutate(cyl = paste0("cyl", cyl))

has_alert <- function(model) {
  # test1: check for any predictions are above the threshold
  test1 <- tidypredict_test(
    model,
    include_intervals = TRUE
  )$alert
  # test2: to see if the if the same results are returned
  # when a parsed model is passed instead of a model
  pm <- parse_model(model)
  test_pm <- df %>% tidypredict_to_column(pm)
  test_original <- df %>% tidypredict_to_column(model)
  test2 <- test_pm != test_original

  any(test1, test2)
}

test_that("Predictions within threshold and parsed model results are equal", {
  expect_false(has_alert(lm(mpg ~ wt, offset = am, data = df)))
  expect_false(has_alert(lm(mpg ~ wt + am + cyl, data = df)))
  expect_false(has_alert(lm(mpg ~ wt + disp * am, data = df)))
  expect_false(has_alert(lm(mpg ~ wt + disp * cyl, data = df)))
  expect_false(has_alert(lm(mpg ~ (wt + disp) * cyl, data = df)))
})
