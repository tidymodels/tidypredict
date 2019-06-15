context("randomForest")
set.seed(100)
rf_model <- randomForest::randomForest(Species ~ ., data = iris, ntree = 100, proximity = TRUE)
tf <- tidypredict_fit(rf_model)
pm <- parse_model(rf_model)

test_that("Returns the correct type and dimensions", {
  expect_is(pm, "list")
  expect_equal(length(pm), 2)
  expect_equal(length(pm$trees), 100)
  expect_equal(pm$general$model, "randomForest")
  expect_equal(pm$general$version, 2)
})

test_that("Returns expected case_when() dplyr formula", {
  expect_equal(
    rlang::expr_text(tf[[1]]),
    "case_when(Petal.Length < 2.5 ~ \"setosa\", Petal.Length >= 5.05 & \n    Petal.Length >= 2.5 ~ \"virginica\", Petal.Width >= 1.9 & Petal.Length < \n    5.05 & Petal.Length >= 2.5 ~ \"virginica\", Sepal.Length < \n    4.95 & Petal.Width < 1.9 & Petal.Length < 5.05 & Petal.Length >= \n    2.5 ~ \"virginica\", Petal.Width < 1.75 & Sepal.Length >= 4.95 & \n    Petal.Width < 1.9 & Petal.Length < 5.05 & Petal.Length >= \n    2.5 ~ \"versicolor\", Sepal.Width < 3 & Petal.Width >= 1.75 & \n    Sepal.Length >= 4.95 & Petal.Width < 1.9 & Petal.Length < \n    5.05 & Petal.Length >= 2.5 ~ \"virginica\", Sepal.Width >= \n    3 & Petal.Width >= 1.75 & Sepal.Length >= 4.95 & Petal.Width < \n    1.9 & Petal.Length < 5.05 & Petal.Length >= 2.5 ~ \"versicolor\")"
  )
})
