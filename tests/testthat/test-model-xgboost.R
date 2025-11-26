test_that("returns the right output", {
  xgb_bin_data <- xgboost::xgb.DMatrix(
    as.matrix(mtcars[, -9]),
    label = mtcars$am
  )

  model <- xgboost::xgb.train(
    params = list(
      max_depth = 2,
      objective = "reg:squarederror",
      base_score = 0.5
    ),
    data = xgb_bin_data,
    nrounds = 4
  )
  tf <- tidypredict_fit(model)
  pm <- parse_model(model)

  expect_type(tf, "language")

  expect_s3_class(pm, "list")
  expect_equal(length(pm), 2)
  expect_equal(length(pm$trees), 4)
  expect_equal(pm$general$model, "xgb.Booster")
  expect_equal(pm$general$version, 1)

  expect_snapshot(
    rlang::expr_text(tf),
    variant = as.character(packageVersion("xgboost"))
  )
})

test_that("Model can be saved and re-loaded", {
  xgb_bin_data <- xgboost::xgb.DMatrix(
    as.matrix(mtcars[, -9]),
    label = mtcars$am
  )

  model <- xgboost::xgb.train(
    params = list(
      max_depth = 2,
      objective = "reg:squarederror",
      base_score = 0.5
    ),
    data = xgb_bin_data,
    nrounds = 4
  )

  pm <- parse_model(model)
  mp <- tempfile(fileext = ".yml")
  yaml::write_yaml(pm, mp)
  l <- yaml::read_yaml(mp)
  pm <- as_parsed_model(l)

  expect_identical(
    round_print(tidypredict_fit(model), digits = 6),
    round_print(tidypredict_fit(pm), digits = 6)
  )
})

test_that("formulas produces correct predictions", {
  xgb_bin_data <- xgboost::xgb.DMatrix(
    as.matrix(mtcars[, -9]),
    label = mtcars$am
  )

  # objective = "reg:squarederror"
  expect_snapshot(
    tidypredict_test(
      xgboost::xgb.train(
        params = list(
          max_depth = 2,
          objective = "reg:squarederror",
          base_score = 0.5
        ),
        data = xgb_bin_data,
        nrounds = 4
      ),
      mtcars,
      xg_df = xgb_bin_data,
      threshold = 0.0000001
    )
  )

  # objective = "binary:logitraw"
  expect_snapshot(
    tidypredict_test(
      xgboost::xgb.train(
        params = list(
          max_depth = 2,
          objective = "binary:logitraw",
          base_score = 0.5
        ),
        data = xgb_bin_data,
        nrounds = 4
      ),
      mtcars,
      xg_df = xgb_bin_data,
      threshold = 0.0000001
    )
  )

  # objective = "reg:logistic"
  expect_snapshot(
    tidypredict_test(
      xgboost::xgb.train(
        params = list(
          max_depth = 2,
          objective = "reg:logistic",
          base_score = 0.5
        ),
        data = xgb_bin_data,
        nrounds = 4
      ),
      mtcars,
      xg_df = xgb_bin_data,
      threshold = 0.0000001
    )
  )

  # objective = "binary:logistic"
  expect_snapshot(
    tidypredict_test(
      xgboost::xgb.train(
        params = list(
          max_depth = 2,
          objective = "binary:logistic",
          base_score = 0.5
        ),
        data = xgb_bin_data,
        nrounds = 4
      ),
      mtcars,
      xg_df = xgb_bin_data,
      threshold = 0.0000001
    )
  )

  # objective = "reg:tweedie"
  expect_snapshot(
    tidypredict_test(
      xgboost::xgb.train(
        params = list(
          max_depth = 2,
          objective = "reg:tweedie",
          base_score = 0.5
        ),
        data = xgb_bin_data,
        nrounds = 4
      ),
      mtcars,
      xg_df = xgb_bin_data,
      threshold = 0.0000001
    )
  )

  # objective = "count:poisson"
  expect_snapshot(
    tidypredict_test(
      xgboost::xgb.train(
        params = list(
          max_depth = 2,
          objective = "count:poisson",
          base_score = 0.5
        ),
        data = xgb_bin_data,
        nrounds = 4
      ),
      mtcars,
      xg_df = xgb_bin_data,
      threshold = 0.0000001
    )
  )

  # objective = "reg:logistic", base_score
  expect_snapshot(
    tidypredict_test(
      xgboost::xgb.train(
        params = list(
          max_depth = 2,
          objective = "reg:logistic",
          base_score = mean(mtcars$am)
        ),
        data = xgb_bin_data,
        nrounds = 4
      ),
      mtcars,
      xg_df = xgb_bin_data,
      threshold = 0.0000001
    )
  )

  # objective = "binary:logistic", base_score
  expect_snapshot(
    tidypredict_test(
      xgboost::xgb.train(
        params = list(
          max_depth = 2,
          objective = "binary:logistic",
          base_score = mean(mtcars$am)
        ),
        data = xgb_bin_data,
        nrounds = 4
      ),
      mtcars,
      xg_df = xgb_bin_data,
      threshold = 0.0000001
    )
  )

  # objective = "reg:logistic", large
  expect_snapshot(
    tidypredict_test(
      xgboost::xgb.train(
        params = list(
          max_depth = 2,
          objective = "reg:logistic",
          base_score = 0.5
        ),
        data = xgb_bin_data,
        nrounds = 50
      ),
      mtcars,
      xg_df = xgb_bin_data,
      threshold = 0.0000001
    )
  )

  # objective = "binary:logistic", large
  expect_snapshot(
    tidypredict_test(
      xgboost::xgb.train(
        params = list(
          max_depth = 2,
          objective = "binary:logistic",
          base_score = 0.5
        ),
        data = xgb_bin_data,
        nrounds = 50
      ),
      mtcars,
      xg_df = xgb_bin_data,
      threshold = 0.0000001
    )
  )

  # objective = "reg:logistic", depp
  expect_snapshot(
    tidypredict_test(
      xgboost::xgb.train(
        params = list(
          max_depth = 20,
          objective = "reg:logistic",
          base_score = 0.5
        ),
        data = xgb_bin_data,
        nrounds = 4
      ),
      mtcars,
      xg_df = xgb_bin_data,
      threshold = 0.0000001
    )
  )

  # objective = "binary:logistic", deep
  expect_snapshot(
    tidypredict_test(
      xgboost::xgb.train(
        params = list(
          max_depth = 20,
          objective = "binary:logistic",
          base_score = 0.5
        ),
        data = xgb_bin_data,
        nrounds = 4
      ),
      mtcars,
      xg_df = xgb_bin_data,
      threshold = 0.0000001
    )
  )
})

test_that("base_score isn't included when 0 (#147)", {
  xgb_bin_data <- xgboost::xgb.DMatrix(
    as.matrix(mtcars[, -9]),
    label = mtcars$am
  )

  model <- xgboost::xgb.train(
    params = list(
      max_depth = 1,
      objective = "reg:squarederror",
      base_score = 0.5
    ),
    data = xgb_bin_data,
    nrounds = 1
  )

  res <- tidypredict_fit(model)
  res <- expr_text(res)
  expect_true(grepl("+ 0.5$", res))

  model <- xgboost::xgb.train(
    params = list(
      max_depth = 1,
      objective = "reg:squarederror",
      base_score = 0
    ),
    data = xgb_bin_data,
    nrounds = 1
  )

  res <- tidypredict_fit(model)
  res <- expr_text(res)
  expect_false(grepl("+ 0$", res))
})
