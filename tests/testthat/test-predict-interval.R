test_that("tidypredict_interval.data.frame errors", {
  expect_error(
    tidypredict_interval(data.frame(x = 1)),
    "no longer supported"
  )
})

test_that("tidypredict_interval.list works for lm", {
  model <- lm(mpg ~ wt + cyl, data = mtcars)
  pm <- parse_model(model)
  interval <- tidypredict_interval(pm)
  expect_type(interval, "language")
})

test_that("tidypredict_interval.list works for glm", {
  model <- glm(mpg ~ wt + cyl, data = mtcars, family = "gaussian")
  pm <- parse_model(model)
  interval <- tidypredict_interval(pm)
  expect_type(interval, "language")
})

test_that("tidypredict_interval.list errors for unsupported model", {
  pm <- list(general = list(model = "unsupported"))
  class(pm) <- "list"
  expect_snapshot(
    error = TRUE,
    tidypredict_interval(pm)
  )
})
