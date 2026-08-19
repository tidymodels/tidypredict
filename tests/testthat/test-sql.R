test_that("Correct SQL query is returned", {
  skip_if_not_installed("dbplyr")
  expect_s3_class(
    tidypredict_sql(lm(mpg ~ wt, data = mtcars), dbplyr::simulate_dbi()),
    "sql"
  )
  expect_s3_class(
    tidypredict_sql_interval(
      lm(mpg ~ wt, data = mtcars),
      dbplyr::simulate_dbi()
    ),
    "sql"
  )
})

test_that("tidypredict_sql() translates the fitted formula", {
  skip_if_not_installed("dbplyr")
  model <- lm(mpg ~ wt + cyl, data = mtcars)
  sql <- as.character(tidypredict_sql(model, dbplyr::simulate_dbi()))

  expect_match(sql, "wt")
  expect_match(sql, "cyl")
})

test_that("tidypredict_sql() accepts a parsed model", {
  skip_if_not_installed("dbplyr")
  model <- lm(mpg ~ wt, data = mtcars)

  expect_equal(
    tidypredict_sql(parse_model(model), dbplyr::simulate_dbi()),
    tidypredict_sql(model, dbplyr::simulate_dbi())
  )
})

test_that("tidypredict_sql() returns one query per formula for multiclass", {
  skip_if_not_installed("dbplyr")
  skip_if_not_installed("MASS")
  model <- MASS::lda(Species ~ ., data = iris)

  sql <- tidypredict_sql(model, dbplyr::simulate_dbi())

  expect_type(sql, "list")
  expect_named(sql, levels(iris$Species))
  expect_s3_class(sql[[1]], "sql")
})

test_that("tidypredict_sql() translates case_when for tree models", {
  skip_if_not_installed("dbplyr")
  skip_if_not_installed("rpart")
  model <- rpart::rpart(mpg ~ wt, data = mtcars)

  sql <- as.character(tidypredict_sql(model, dbplyr::simulate_dbi()))

  expect_match(sql, "CASE")
})

test_that("tidypredict_sql_interval() widens with `interval`", {
  skip_if_not_installed("dbplyr")
  model <- lm(mpg ~ wt, data = mtcars)
  con <- dbplyr::simulate_dbi()

  expect_false(identical(
    as.character(tidypredict_sql_interval(model, con)),
    as.character(tidypredict_sql_interval(model, con, interval = 0.99))
  ))
})

test_that("tidypredict_sql() returns one query for intercept-only models (#313)", {
  skip_if_not_installed("dbplyr")
  sql <- tidypredict_sql(lm(mpg ~ 1, data = mtcars), dbplyr::simulate_dbi())

  expect_s3_class(sql, "sql")
  expect_length(sql, 1)
})

test_that("tidypredict_sql_interval() validates `interval` (#313)", {
  skip_if_not_installed("dbplyr")
  model <- lm(mpg ~ wt, data = mtcars)

  expect_snapshot(
    error = TRUE,
    tidypredict_sql_interval(model, dbplyr::simulate_dbi(), interval = 1.5)
  )
})

test_that("tidypredict_sql_interval() errors for unsupported models", {
  skip_if_not_installed("dbplyr")
  skip_if_not_installed("rpart")
  model <- rpart::rpart(mpg ~ wt, data = mtcars)

  expect_snapshot(
    error = TRUE,
    tidypredict_sql_interval(model, dbplyr::simulate_dbi())
  )
})
