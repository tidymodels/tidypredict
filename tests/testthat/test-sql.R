test_that("Correct SQL query is returned", {
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
