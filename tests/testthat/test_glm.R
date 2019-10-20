context("glm")

df <- mtcars
df$cyl <- paste0("cyl", df$cyl)

has_alert <- function(...) tidypredict_test(...)$alert

test_that("Individual prediction difference is never above 1e-12", {
  expect_false(has_alert(glm(am ~ wt + cyl, data = df, family = "gaussian")))
  expect_false(has_alert(glm(am ~ wt + disp, data = df, family = "gaussian")))
  expect_false(has_alert(glm(am ~ wt + cyl, data = df, family = "binomial")))
  expect_false(has_alert(glm(am ~ wt + disp + cyl, data = df, family = "binomial")))
})

test_that("Intervals return a call", {
  expect_is(
    tidypredict_interval(glm(am ~ cyl * wt + mpg, data = mtcars, family = "gaussian")),
    "call"
  )
})

context("glm-saved")
test_that("Model can be saved and re-loaded", {
  model <- glm(am ~ wt + disp + cyl, data = df, family = "binomial")
  mp <- tempfile(fileext = ".yml")
  yaml::write_yaml(parse_model(model), mp)
  l <- yaml::read_yaml(mp)
  pm <- as_parsed_model(l)
  expect_silent(tidypredict_fit(pm))
})
