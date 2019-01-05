context("test-list")

fitted <- function(...) tidypredict_fit(parse_model(...))

test_that("Supports parsed models in list objects", {
  expect_equal(
    fitted(lm(mpg~wt, data = mtcars)),
    expr(37.285126167342 + (wt * -5.34447157272267))
  )
  
  expect_equal(
    length(fitted(
      randomForest::randomForest(Species~., data = iris)
    )),
    500
  )
})
