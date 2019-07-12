context("lm")

df <- mtcars
df$cyl <- paste0("cyl", df$cyl)

has_alert <- function(model) tidypredict_test(model, df = df, include_intervals = TRUE)$alert

test_that("Predictions within threshold and parsed model results are equal", {
  expect_false(has_alert(lm(mpg ~ wt, offset = am, data = df)))
  expect_false(has_alert(lm(mpg ~ wt + am + cyl, data = df)))
  expect_false(has_alert(lm(mpg ~ wt + disp * am, data = df)))
  expect_false(has_alert(lm(mpg ~ wt + disp * cyl, data = df)))
  expect_false(has_alert(lm(mpg ~ (wt + disp) * cyl, data = df)))
})

context("lm-parsnip")

lm_parsnip <- function(...) {
  parsnip::fit(
    parsnip::set_engine(parsnip::linear_reg(), "lm"),
    ...
  )
}

test_that("Predictions within threshold and parsed model results are equal", {
  expect_false(has_alert(lm_parsnip(mpg ~ wt, offset = am, data = df)))
  expect_false(has_alert(lm_parsnip(mpg ~ wt + am + cyl, data = df)))
  expect_false(has_alert(lm_parsnip(mpg ~ wt + disp * am, data = df)))
  expect_false(has_alert(lm_parsnip(mpg ~ wt + disp * cyl, data = df)))
  expect_false(has_alert(lm_parsnip(mpg ~ (wt + disp) * cyl, data = df)))
})

context("lm-saved")
test_that("Model can be saved and re-loaded", {
  model <- lm(mpg ~ (wt + disp) * cyl, data = df)
  mp <- tempfile(fileext = ".yml")
  yaml::write_yaml(parse_model(model), mp)
  l <- yaml::read_yaml(mp)
  pm <- as_parsed_model(l)
  expect_silent(tidypredict_fit(pm))
})

test_that("tidy() works", {
  expect_is(
    tidy(parse_model(lm(mpg ~., mtcars))),
    "tbl_df"
  )  
})
