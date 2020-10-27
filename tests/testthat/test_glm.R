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

test_that("tidypredict works when variable names are subset of other variables", {
  df2 <- df
  df2$wt_sq <- df2$wt ^ 2
  df2$char_cyl = as.character(df2$cyl)
  set.seed(22)
  df2$char_cyl_2 = sample(letters[1:3], size = nrow(df2), replace = TRUE)
  model4 <- suppressWarnings(glm(
    am ~ wt + wt_sq + char_cyl + char_cyl_2, 
    data = df2, family = "binomial"
  ))
  expect_silent(tidypredict_fit(model4))
  expect_false(tidypredict_test(model4)$alert)
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
