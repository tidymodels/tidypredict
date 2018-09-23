context("earth/MARS models")

test_earth <- function(..., .data = mtcars){
  p <- invisible(
    tidypredict_test(earth::earth(...), df = .data)
  )
  p$alert
}

test_that("Simple models", {
  expect_false(
    test_earth(mpg ~ wt, data = mtcars)
  )  
})

test_that("Models w degree work", {
  expect_false(
    test_earth(mpg ~ wt, data = mtcars, degree = 2)
    )
  expect_false(
    test_earth(mpg ~ wt, data = mtcars, degree = 3)
    )  
})

test_that("Most pmethods work", {
  pmethods <- c("backward", "none", "exhaustive", "forward", "seqrep") 
  res <- lapply(
    pmethods,
    function(x){
      test_earth(mpg ~ wt, data = mtcars, pmethod = x)
    }
  )
  expect_false(any(as.logical(res)))
})

test_that("nfold and cv pmethod work", {
  expect_false(
    test_earth(mpg ~ wt, data = mtcars, nfold = 10, pmethod = "cv")
  )
})
