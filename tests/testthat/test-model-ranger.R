test_that("ranger works", {
  model <- parsnip::fit(
    parsnip::set_engine(
      parsnip::rand_forest(trees = 10, mode = "classification"),
      "ranger",
      seed = 100,
      num.threads = 2
    ),
    Species ~ .,
    data = iris
  )

  tf <- tidypredict_fit(model)
  pm <- parse_model(model)

  expect_s3_class(pm, "list")
  expect_equal(length(pm), 2)
  expect_equal(length(pm$trees), 10)
  expect_equal(pm$general$model, "ranger")
  expect_equal(pm$general$version, 2)
})

test_that("Model can be saved and re-loaded", {
  model <- ranger::ranger(
    Species ~ .,
    data = iris,
    num.trees = 10,
    seed = 100,
    num.threads = 2
  )
  mp <- tempfile(fileext = ".yml")
  yaml::write_yaml(parse_model(model), mp)
  l <- yaml::read_yaml(mp)
  pm <- as_parsed_model(l)
  expect_silent(tidypredict_fit(pm))
})
