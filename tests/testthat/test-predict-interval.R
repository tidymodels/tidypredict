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

test_that("tidypredict_interval() validates `interval` (#313)", {
  model <- lm(mpg ~ wt, data = mtcars)
  pm <- parse_model(model)

  expect_snapshot(error = TRUE, tidypredict_interval(model, 1.5))
  expect_snapshot(error = TRUE, tidypredict_interval(model, 0))
  expect_snapshot(error = TRUE, tidypredict_interval(model, "a"))
  expect_snapshot(error = TRUE, tidypredict_interval(model, c(0.9, 0.95)))
  expect_snapshot(
    error = TRUE,
    tidypredict_interval(glm(am ~ wt, mtcars, family = "binomial"), 2)
  )
  expect_snapshot(error = TRUE, tidypredict_interval(pm, 1.5))
})

test_that("tidypredict_interval.list errors on a malformed parsed model (#313)", {
  expect_snapshot(error = TRUE, tidypredict_interval(list()))
  expect_snapshot(
    error = TRUE,
    tidypredict_interval(structure(list(general = list()), class = "list"))
  )
})

test_that("tidypredict_interval() errors for a parsed tree model (#313)", {
  skip_if_not_installed("rpart")
  pm <- parse_model(rpart::rpart(mpg ~ wt, data = mtcars))

  expect_snapshot(error = TRUE, tidypredict_interval(pm))
})
