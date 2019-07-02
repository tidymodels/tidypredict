context("ranger")
set.seed(100)
rf_model <- ranger::ranger(Species ~ ., data = iris, num.trees = 100)
tf <- tidypredict_fit(rf_model)
pm <- parse_model(rf_model)


test_that("Returns the correct type and dimensions", {
  expect_is(pm, "list")
  expect_equal(length(pm), 2)
  expect_equal(length(pm$trees), 100)
  expect_equal(pm$general$model, "ranger")
  expect_equal(pm$general$version, 2)
})

test_that("Returns expected case_when() dplyr formula", {
  expect_equal(
    rlang::expr_text(tf[[1]]),
    "case_when(Petal.Length < 2.45 & Petal.Width < 1.75 ~ \"setosa\", \n    Petal.Length < 4.85 & Petal.Width >= 1.75 ~ \"virginica\", \n    Petal.Length >= 4.85 & Petal.Width >= 1.75 ~ \"virginica\", \n    Petal.Length < 5.4 & Petal.Length >= 2.45 & Petal.Width < \n        1.75 ~ \"versicolor\", Petal.Length >= 5.4 & Petal.Length >= \n        2.45 & Petal.Width < 1.75 ~ \"virginica\")"
  )
})

