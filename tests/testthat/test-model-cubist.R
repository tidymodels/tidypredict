test_that("returns the right output", {
  model <- Cubist::cubist(
    x = mtcars[, -1],
    y = mtcars$mpg,
    committees = 3
  )
  tf <- tidypredict_fit(model)
  pm <- parse_model(model)

  expect_type(tf, "language")

  expect_s3_class(pm, "list")
  expect_equal(length(pm), 2)
  expect_equal(length(pm$trees), 1)
  expect_equal(pm$general$model, "cubist")
  expect_equal(pm$general$version, 2)

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

test_that("predictions are the same", {
  model <- Cubist::cubist(
    x = mtcars[, -1],
    y = mtcars$mpg,
    committees = 3
  )
  tf <- tidypredict_fit(model)

  splits <- dplyr::distinct(model$splits, variable, value)
  splits <- map2(splits$variable, splits$value, function(x, y) {
    str2lang(paste(x, "!=", y))
  })

  non_split_data <- mtcars |>
    dplyr::filter(!!!splits)

  expect_equal(
    with(non_split_data, eval(tf)),
    predict(model, non_split_data),
    tolerance = 0.0001
  )
})

test_that("intercept is done correctly (#58)", {
  biomass_tr <- modeldata::biomass %>%
    dplyr::filter(dataset == "Training") %>%
    dplyr::select(-dataset, -sample)

  set.seed(1)
  mod <- Cubist::cubist(
    x = biomass_tr %>% dplyr::select(-HHV),
    y = biomass_tr$HHV
  )

  res <- tidypredict_fit(mod)
  expect_false(any(grepl("list", res)))
})
