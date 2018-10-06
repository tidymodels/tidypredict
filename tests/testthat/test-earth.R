context("earth/MARS models")

data("etitanic", package = "earth")

test_earth <- function(..., .data = etitanic) {
  p <- invisible(
    tidypredict_test(earth::earth(...), df = .data)
  )
  p$alert
}

test_that("Simple models", {
  expect_false(
    test_earth(age ~ sibsp + parch, data = etitanic)
  )
})

test_that("Less simple models w categorical vars", {
  expect_false(
    test_earth(age ~ ., data = etitanic)
  )
})

test_that("Models w degree work", {
  expect_false(test_earth(age ~ sibsp + parch,
    data = etitanic, degree = 2
  ))
  expect_false(test_earth(age ~ sibsp + parch,
    data = etitanic, degree = 3
  ))
})

test_that("Most pmethods work", {
  pmethods <- c("backward", "none", "exhaustive", "forward", "seqrep")
  res <- lapply(
    pmethods,
    function(x) {
      expect_false(test_earth(age ~ sibsp + parch,
        data = etitanic, pmethod = x
      ))
    }
  )
  expect_false(any(as.logical(res)))
})

context("earth/MARS models - binomial")

test_that("simple binomial works", {
  expect_false(
    test_earth(survived ~ age + sibsp,
      data = etitanic,
      glm = list(family = binomial), .data = etitanic
    )
  )
})

test_that("complex binomial w/ degree works", {
  expect_false(
    test_earth(survived ~ .,
      data = etitanic,
      glm = list(family = binomial), degree = 2, .data = etitanic
    )
  )
})

test_that("Most pmethods work", {
  pmethods <- c("backward", "exhaustive", "forward", "seqrep")
  res <- lapply(
    pmethods,
    function(x) {
      test_earth(survived ~ age + sibsp,
        data = etitanic,
        pmethod = x, glm = list(family = binomial)
      )
    }
  )
  expect_false(any(as.logical(res)))
})
