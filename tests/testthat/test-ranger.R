num_trees <- 20

run_test <- function(model, test_formula = TRUE) {
  tf <- tidypredict_fit(model)
  pm <- parse_model(model)
  test_that("Returns the correct type and dimensions", {
    expect_is(pm, "list")
    expect_equal(length(pm), 2)
    expect_equal(length(pm$trees), num_trees)
    expect_equal(pm$general$model, "ranger")
    expect_equal(pm$general$version, 2)
  })
  if(test_formula) {
    test_that("Returns expected case_when() dplyr formula", {
      expect_equal(
        rlang::expr_text(tf[[1]]),
        "case_when(Petal.Length < 2.45 ~ \"setosa\", Petal.Width < 1.45 & \n    Petal.Width < 1.65 & Petal.Length >= 2.45 ~ \"versicolor\", \n    Sepal.Length < 6.55 & Petal.Width >= 1.65 & Petal.Length >= \n        2.45 ~ \"virginica\", Petal.Length < 4.95 & Petal.Width >= \n        1.45 & Petal.Width < 1.65 & Petal.Length >= 2.45 ~ \"versicolor\", \n    Petal.Width < 1.75 & Sepal.Length >= 6.55 & Petal.Width >= \n        1.65 & Petal.Length >= 2.45 ~ \"versicolor\", Petal.Width >= \n        1.75 & Sepal.Length >= 6.55 & Petal.Width >= 1.65 & Petal.Length >= \n        2.45 ~ \"virginica\", Sepal.Length >= 6.15 & Petal.Length >= \n        4.95 & Petal.Width >= 1.45 & Petal.Width < 1.65 & Petal.Length >= \n        2.45 ~ \"virginica\", Sepal.Width < 2.45 & Sepal.Length < \n        6.15 & Petal.Length >= 4.95 & Petal.Width >= 1.45 & Petal.Width < \n        1.65 & Petal.Length >= 2.45 ~ \"virginica\", Sepal.Width >= \n        2.45 & Sepal.Length < 6.15 & Petal.Length >= 4.95 & Petal.Width >= \n        1.45 & Petal.Width < 1.65 & Petal.Length >= 2.45 ~ \"versicolor\")"
      )
    })
  }
}

context("ranger")
run_test(
  ranger::ranger(Species ~ ., data = iris, num.trees = num_trees, seed = 100)
)

context("ranger-classification")
run_test(
  parsnip::fit(
    parsnip::set_engine(parsnip::rand_forest(trees = num_trees, mode = "classification"), "ranger", seed = 100),
    Species ~ .,
    data = iris
  ), test_formula = FALSE
)

context("ranger-parsnip")
run_test(
  parsnip::fit(
    parsnip::set_engine(parsnip::rand_forest(trees = num_trees), "ranger", seed = 100),
    Species ~ .,
    data = iris
  )
)

context("ranger-saved")
test_that("Model can be saved and re-loaded", {
  model <- ranger::ranger(Species ~ ., data = iris, num.trees = num_trees, seed = 100)
  mp <- tempfile(fileext = ".yml")
  yaml::write_yaml(parse_model(model), mp)
  l <- yaml::read_yaml(mp)
  pm <- as_parsed_model(l)
  expect_silent(tidypredict_fit(pm))
  
})
