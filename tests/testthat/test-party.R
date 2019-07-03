context("party")

df <- mtcars
has_alert <- function(model) tidypredict_test(model, df = df)$alert

test_that("Predictions within threshold and parsed model results are equal", {
  expect_false(has_alert(partykit::ctree(mpg ~ wt, offset = am, data = df)))
  expect_false(has_alert(partykit::ctree(mpg ~ wt + am + cyl, data = df)))
  expect_false(has_alert(partykit::ctree(mpg ~ wt + disp * am, data = df)))
  expect_false(has_alert(partykit::ctree(mpg ~ wt + disp * cyl, data = df)))
  expect_false(has_alert(partykit::ctree(mpg ~ (wt + disp) * cyl, data = df)))
})
