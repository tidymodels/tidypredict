test_that("tidypredict_fit.pm_tree works for v3 party", {
  skip_if_not_installed("partykit")
  model <- partykit::ctree(mpg ~ wt + cyl, data = mtcars)
  pm <- parse_model(model)
  fit <- tidypredict_fit(pm)
  expect_type(fit, "language")
})

test_that("tidypredict_fit.pm_tree works for v3 ranger", {
  skip_if_not_installed("ranger")
  model <- ranger::ranger(
    mpg ~ wt + cyl,
    data = mtcars,
    num.trees = 2,
    num.threads = 1
  )
  pm <- parse_model(model)
  fit <- tidypredict_fit(pm)
  expect_type(fit, "language")
})

test_that("tidypredict_fit.pm_tree works for v3 randomForest", {
  skip_if_not_installed("randomForest")
  model <- randomForest::randomForest(mpg ~ wt + cyl, data = mtcars, ntree = 2)
  pm <- parse_model(model)
  fit <- tidypredict_fit(pm)
  expect_type(fit, "language")
})

test_that("tidypredict_fit.pm_tree works for v2 ranger (backwards compat)", {
  pm <- readRDS(test_path("backwards-compat", "ranger-v2-parsed.rds"))
  fit <- tidypredict_fit(pm)
  expect_type(fit, "language")
})

test_that("tidypredict_fit.pm_tree works for v2 cubist (backwards compat)", {
  pm <- readRDS(test_path("backwards-compat", "cubist-v2-regression.rds"))
  fit <- tidypredict_fit(pm)
  expect_type(fit, "language")
})

test_that("tidypredict_fit.pm_tree works for v2 randomForest (backwards compat)", {
  pm <- readRDS(test_path("backwards-compat", "rf-v2-regression.rds"))
  fit <- tidypredict_fit(pm)
  expect_type(fit, "language")
})

test_that("tidypredict_fit.pm_tree works for v2 party (backwards compat)", {
  skip_if_not_installed("partykit")
  pm <- readRDS(test_path("backwards-compat", "party-v2-regression.rds"))
  model <- partykit::ctree(mpg ~ wt + disp + hp, data = mtcars)

  fit <- tidypredict_fit(pm)
  res <- dplyr::mutate(mtcars, .fit = !!fit)$.fit

  expect_equal(res, unname(predict(model, mtcars)))
})

test_that("tidypredict_fit.pm_tree works for v2 rpart surrogates (backwards compat)", {
  skip_if_not_installed("rpart")
  pm <- readRDS(test_path("backwards-compat", "rpart-v2-surrogates.rds"))
  data <- readRDS(test_path("backwards-compat", "rpart-v2-data.rds"))
  model <- rpart::rpart(
    mpg ~ wt + disp + hp + drat,
    data = data,
    control = rpart::rpart.control(minsplit = 5, cp = 0.01)
  )

  fit <- tidypredict_fit(pm)
  res <- dplyr::mutate(data, .fit = !!fit)$.fit

  expect_equal(res, unname(predict(model, data)))
})

test_that("tidypredict_fit.pm_tree works for v2 rpart without surrogates (backwards compat)", {
  skip_if_not_installed("rpart")
  pm <- readRDS(test_path("backwards-compat", "rpart-v2-nosurrogate.rds"))
  data <- readRDS(test_path("backwards-compat", "rpart-v2-data.rds"))
  model <- rpart::rpart(
    mpg ~ wt + disp + hp + drat,
    data = data,
    control = rpart::rpart.control(minsplit = 5, cp = 0.01, usesurrogate = 0)
  )

  fit <- tidypredict_fit(pm)
  res <- dplyr::mutate(data, .fit = !!fit)$.fit

  expect_equal(res, unname(predict(model, data)))
})

test_that("tidypredict_to_column() and tidypredict_sql() work for v2 party", {
  skip_if_not_installed("partykit")
  skip_if_not_installed("dbplyr")
  pm <- readRDS(test_path("backwards-compat", "party-v2-regression.rds"))
  model <- partykit::ctree(mpg ~ wt + disp + hp, data = mtcars)

  res <- tidypredict_to_column(mtcars, pm)

  expect_equal(res$fit, unname(predict(model, mtcars)))
  expect_s3_class(tidypredict_sql(pm, dbplyr::simulate_dbi()), "sql")
})

test_that("tidypredict_fit.pm_tree errors for unsupported v2 models", {
  pm <- structure(
    list(general = list(model = "made_up", version = 2, type = "tree")),
    class = c("parsed_model", "pm_tree", "list")
  )
  expect_snapshot(error = TRUE, tidypredict_fit(pm))
})

test_that("tidypredict_fit() errors for a model class it has no parser for", {
  expect_snapshot(
    error = TRUE,
    tidypredict_fit(structure(list(), class = "made_up_model"))
  )
})

test_that("tidypredict_fit() errors for a parsed model type with no builder", {
  pm <- as_parsed_model(list(general = list(type = "made_up")))
  expect_snapshot(error = TRUE, tidypredict_fit(pm))
})

test_that("unsupported models are signalled with a distinguishing class", {
  # Callers such as orbital need to tell "no method exists for this model"
  # apart from "this model is supported but this configuration is not", which
  # share the "are not supported" wording.
  expect_error(
    tidypredict_fit(structure(list(), class = "made_up_model")),
    class = "tidypredict_unsupported_model"
  )

  expect_error(
    parse_model(structure(list(), class = "made_up_model")),
    class = "tidypredict_unsupported_model"
  )

  # The parsed-model branch of the same helper.
  expect_error(
    parse_model(as_parsed_model(list(general = list(type = "made_up")))),
    class = "tidypredict_unsupported_model"
  )
})
