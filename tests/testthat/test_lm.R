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

test_that("tidypredict works when variable names are subset of other variables", {
  df2 <- df
  df2$wt_sq <- df2$wt^2
  df2$char_cyl <- as.character(df2$cyl)
  df2$char_cyl_2 <- sample(letters[1:3], size = nrow(df2), replace = TRUE)
  model4 <- lm(
    am ~ wt + wt_sq + char_cyl + char_cyl_2,
    data = df2
  )

  expect_silent(tidypredict_fit(model4))
  expect_false(tidypredict_test(model4)$alert)
})

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

test_that("Model can be saved and re-loaded", {
  model <- lm(mpg ~ (wt + disp) * cyl, data = df)
  mp <- tempfile(fileext = ".yml")
  yaml::write_yaml(parse_model(model), mp)
  l <- yaml::read_yaml(mp)
  pm <- as_parsed_model(l)
  expect_snapshot(tidypredict_fit(pm))
})

test_that("tidy() works", {
  expect_s3_class(
    tidy(parse_model(lm(mpg ~ ., mtcars))),
    "tbl_df"
  )
})
