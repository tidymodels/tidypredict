fitted_rf <- function(...) tidypredict_fit(parse_model(...))

test_that("Supports parsed models in list objects", {
  expect_equal(
    class(fitted_rf(lm(mpg ~ wt, data = mtcars)))[1],
    "call"
  )
  expect_equal(
    length(fitted_rf(
      randomForest::randomForest(Species ~ ., data = iris)
    )),
    500
  )
})
