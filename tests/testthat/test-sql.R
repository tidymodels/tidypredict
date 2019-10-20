context("sql")

test_that("Correct SQL query is returned", {
  expect_is(
    tidypredict_sql(lm(mpg ~ wt, data = mtcars), dbplyr::simulate_dbi()),
    "sql"
  )
  expect_is(
    tidypredict_sql_interval(lm(mpg ~ wt, data = mtcars), dbplyr::simulate_dbi()),
    "sql"
  )
})
