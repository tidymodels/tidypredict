test_that("tidypredict_to_column() adds a fit column matching predict()", {
  model <- lm(mpg ~ wt + cyl, data = mtcars)
  res <- tidypredict_to_column(mtcars, model)

  expect_named(res, c(names(mtcars), "fit"))
  expect_equal(res$fit, unname(predict(model, mtcars)))
})

test_that("tidypredict_to_column() adds interval columns", {
  model <- lm(mpg ~ wt, data = mtcars)
  res <- tidypredict_to_column(mtcars, model, add_interval = TRUE)

  expect_named(res, c(names(mtcars), "fit", "upper", "lower"))
  expect_true(all(res$upper > res$fit))
  expect_true(all(res$lower < res$fit))

  base <- predict(model, mtcars, interval = "prediction", level = 0.95)
  expect_equal(res$upper, unname(base[, "upr"]))
  expect_equal(res$lower, unname(base[, "lwr"]))
})

test_that("tidypredict_to_column() honours `interval`", {
  model <- lm(mpg ~ wt, data = mtcars)
  narrow <- tidypredict_to_column(mtcars, model, add_interval = TRUE)
  wide <- tidypredict_to_column(
    mtcars,
    model,
    add_interval = TRUE,
    interval = 0.99
  )

  expect_true(all(wide$upper > narrow$upper))
  expect_true(all(wide$lower < narrow$lower))
})

test_that("tidypredict_to_column() honours `vars`", {
  model <- lm(mpg ~ wt, data = mtcars)
  res <- tidypredict_to_column(
    mtcars,
    model,
    add_interval = TRUE,
    vars = c("f", "u", "l")
  )

  expect_named(res, c(names(mtcars), "f", "u", "l"))
})

test_that("tidypredict_to_column() accepts a parsed model", {
  model <- lm(mpg ~ wt + cyl, data = mtcars)
  res <- tidypredict_to_column(mtcars, parse_model(model))

  expect_equal(res$fit, unname(predict(model, mtcars)))
})

test_that("tidypredict_to_column() works on a database backend", {
  skip_if_not_installed("dbplyr")
  model <- lm(mpg ~ wt + cyl, data = mtcars)

  db <- dbplyr::tbl_lazy(mtcars, con = dbplyr::simulate_dbi())
  expect_no_error(tidypredict_to_column(db, model))
})

test_that("tidypredict_to_column() errors for models returning many formulas", {
  skip_if_not_installed("MASS")
  model <- MASS::lda(Species ~ ., data = iris)

  expect_snapshot(error = TRUE, tidypredict_to_column(iris, model))
})
