skip_if_not_installed("randomForest")

run_tests <- function(model) {
  tf <- tidypredict_fit(model)
  pm <- parse_model(model)

  test_that("Returns the correct type and dimensions", {
    expect_s3_class(pm, "list")
    expect_equal(length(pm), 2)
    expect_equal(length(pm$trees), 100)
    expect_equal(pm$general$model, "randomForest")
    expect_equal(pm$general$version, 2)
  })
  test_that("Returns expected case_when() dplyr formula", {
    expect_snapshot(
      rlang::expr_text(tf[[1]])
    )
  })
}

set.seed(100)
run_tests(
  randomForest::randomForest(Species ~ ., data = iris, ntree = 100)
)

set.seed(100)
run_tests(
  parsnip::fit(
    parsnip::set_engine(parsnip::rand_forest(trees = 100, mode = "classification"), "randomForest"),
    Species ~ .,
    data = iris
  )
)

test_that("Model can be saved and re-loaded", {
  model <- randomForest::randomForest(Species ~ ., data = iris, ntree = 100)
  mp <- tempfile(fileext = ".yml")
  yaml::write_yaml(parse_model(model), mp)
  l <- yaml::read_yaml(mp)
  pm <- as_parsed_model(l)
  expect_snapshot(tidypredict_fit(pm))
})
