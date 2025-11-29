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
  expect_equal(pm$general$version, 3)

  expect_snapshot(
    rlang::expr_text(tf)
  )
})

test_that("Model can be saved and re-loaded", {
  model <- Cubist::cubist(
    x = mtcars[, -1],
    y = mtcars$mpg,
    committees = 3
  )
  pm <- parse_model(model)
  mp <- tempfile(fileext = ".yml")
  yaml::write_yaml(pm, mp)
  l <- yaml::read_yaml(mp)
  pm <- as_parsed_model(l)

  expect_identical(
    tidypredict_fit(model),
    tidypredict_fit(pm)
  )
})

test_that("formulas produces correct predictions", {
  model <- Cubist::cubist(
    x = mtcars[, -1],
    y = mtcars$mpg,
    committees = 3
  )

  # Cubist doesn't work near splits
  # https://github.com/topepo/Cubist/issues/62
  splits <- dplyr::distinct(model$splits, variable, value)
  splits <- map2(splits$variable, splits$value, function(x, y) {
    str2lang(paste("abs(", x, "-", y, ") > 0.0001"))
  })
  non_split_data <- mtcars %>%
    dplyr::filter(!!!splits)

  expect_snapshot(
    tidypredict_test(
      model,
      non_split_data,
      threshold = 0.00001
    )
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

test_that("doesn't divide by TRUE (#143)", {
  rules <- list(quote(37.2 + hp * -0.0318 + wt * -3.88))
  paths <- list(TRUE)

  expect_identical(
    expr_text(make_committee(rules, paths)),
    "37.2 + hp * -0.0318 + wt * -3.88"
  )

  rules <- list(quote(37.2 + hp * -0.0318 + wt * -3.88))
  paths <- list(quote(disp > 95.099998))

  expect_identical(
    expr_text(make_committee(rules, paths)),
    "(37.2 + hp * -0.0318 + wt * -3.88)/(disp > 95.099998)"
  )
})
