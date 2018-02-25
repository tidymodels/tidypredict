context("glm")

df <- mtcars %>%
  mutate(cyl = paste0("cyl", cyl))

has_alert <- function(model) {
  tidypredict_test(
    model
  )$alert
}

test_that("Individual prediction difference is never above 1e-12", {
  expect_false(has_alert(glm(am ~ wt + cyl, data = df, family = "gaussian")))
  expect_false(has_alert(glm(am ~ wt + disp, data = df, family = "gaussian")))
  expect_false(has_alert(glm(am ~ wt + cyl, data = df, family = "binomial")))
  expect_false(has_alert(glm(am ~ wt + disp + cyl, data = df, family = "binomial")))
})
