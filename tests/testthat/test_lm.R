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

test_that("Intervals are within the threshold", {
  pm <- parse_model(lm(mpg ~ am + wt, data = mtcars))
  t <- tidypredict_interval(pm)
  expected <- rlang::expr(2.0452296421327 * sqrt(-0.176776695296637 * -0.176776695296637 * 
                           9.59723093870482 + (0.1462244129664 + (am) * (-0.359937016532678)) * 
                           (0.1462244129664 + (am) * (-0.359937016532678)) * 9.59723093870482 + 
                           (-0.958962404795433 + (am) * (0.345504476304964) + (wt) * 
                              (0.25444128099978)) * (-0.958962404795433 + (am) * (0.345504476304964) + 
                                                       (wt) * (0.25444128099978)) * 9.59723093870482 + 9.59723093870482))
  expect_equal(t, expected)
})
