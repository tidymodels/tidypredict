set.seed(100)
data("BostonHousing", package = "mlbench")
model <- Cubist::cubist(x = BostonHousing[, -14], y = BostonHousing$medv, committees = 3)
tf <- tidypredict_fit(model)
pm <- parse_model(model)

test_that("Returns the correct type and dimensions", {
  expect_s3_class(pm, "list")
  expect_equal(length(pm), 2)
  expect_equal(length(pm$trees), 1)
  expect_equal(pm$general$model, "cubist")
  expect_equal(pm$general$version, 2)
})

test_that("Returns expected dplyr formula", {
  expect_snapshot(
    rlang::expr_text(tf)
  )
})

test_that("Model can be saved and re-loaded", {
  mp <- tempfile(fileext = ".yml")
  yaml::write_yaml(pm, mp)
  l <- yaml::read_yaml(mp)
  pm <- as_parsed_model(l)
  expect_snapshot(tidypredict_fit(pm))
})
