run_tests <- function(model) {
  tf <- tidypredict_fit(model)
  pm <- parse_model(model)

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
}

context("randomForest")
set.seed(100)
run_tests(
  randomForest::randomForest(Species ~ ., data = iris, ntree = 100)
)

context("randomForest-parsnip")
set.seed(100)
run_tests(
  parsnip::fit(
    parsnip::set_engine(parsnip::rand_forest(trees = 100, mode = "classification"), "randomForest"),
    Species ~ .,
    data = iris
  )
)

context("randomForest-saved")
test_that("Model can be saved and re-loaded", {
  model <- randomForest::randomForest(Species ~ ., data = iris, ntree = 100)
  mp <- tempfile(fileext = ".yml")
  yaml::write_yaml(parse_model(model), mp)
  l <- yaml::read_yaml(mp)
  pm <- as_parsed_model(l)
  expect_silent(tidypredict_fit(pm))
})
