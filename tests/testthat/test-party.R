df <- mtcars
df$am1 <- df$am
df$am <- ifelse(df$am == 1, "auto", "man")
df$am <- as.factor(df$am)

df$cyl <- ifelse(df$cyl == 4, "four", df$cyl)
df$cyl <- ifelse(df$cyl == 6, "six", df$cyl)
df$cyl <- ifelse(df$cyl == 8, "eight", df$cyl)
df$cyl <- as.factor(df$cyl)


has_alert <- function(model) tidypredict_test(model, df = df)$alert

test_that("Predictions within threshold and parsed model results are equal", {
  expect_false(has_alert(partykit::ctree(mpg ~ am + cyl, data = df)))
  expect_false(has_alert(partykit::ctree(mpg ~ wt, offset = am1, data = df)))
  expect_false(has_alert(partykit::ctree(mpg ~ wt + am + cyl, data = df)))
  expect_false(has_alert(partykit::ctree(mpg ~ wt + disp * am, data = df)))
  expect_false(has_alert(partykit::ctree(mpg ~ wt + disp * cyl, data = df)))
  expect_false(has_alert(partykit::ctree(mpg ~ (wt + disp) * cyl, data = df)))
  
})

test_that("Model can be saved and re-loaded", {
  model <- partykit::ctree(mpg ~ wt, offset = am, data = df)
  mp <- tempfile(fileext = ".yml")
  yaml::write_yaml(parse_model(model), mp)
  l <- yaml::read_yaml(mp)
  pm <- as_parsed_model(l)
  expect_snapshot(tidypredict_fit(pm))
})
