num_trees <- 10

run_test <- function(model, test_formula = TRUE) {
  tf <- tidypredict_fit(model)
  pm <- parse_model(model)
  test_that("Returns the correct type and dimensions", {
    expect_s3_class(pm, "list")
    expect_equal(length(pm), 2)
    expect_equal(length(pm$trees), num_trees)
    expect_equal(pm$general$model, "ranger")
    expect_equal(pm$general$version, 2)
  })
  if(test_formula) {
    test_that("Returns expected case_when() dplyr formula", {
      expect_type(tidypredict_fit(pm), "list")
    })
  }
}

run_test(
  ranger::ranger(Species ~ ., data = iris, num.trees = num_trees, seed = 100, num.threads = 2)
)

run_test(
  parsnip::fit(
    parsnip::set_engine(parsnip::rand_forest(trees = num_trees, mode = "classification"), "ranger", seed = 100, num.threads = 2),
    Species ~ .,
    data = iris
  ), test_formula = FALSE
)

run_test(
  parsnip::fit(
    parsnip::set_engine(parsnip::rand_forest(trees = num_trees, mode = "classification"), "ranger", seed = 100, num.threads = 2),
    Species ~ .,
    data = iris
  )
)

test_that("Model can be saved and re-loaded", {
  model <- ranger::ranger(Species ~ ., data = iris, num.trees = num_trees, seed = 100, num.threads = 2)
  mp <- tempfile(fileext = ".yml")
  yaml::write_yaml(parse_model(model), mp)
  l <- yaml::read_yaml(mp)
  pm <- as_parsed_model(l)
  expect_silent(tidypredict_fit(pm))
})
