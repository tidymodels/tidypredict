context("xgboost")

skip_if_not_installed("xgboost")

logregobj <- function(preds, dtrain) {
  labels <- xgboost::getinfo(dtrain, "label")
  preds <- 1 / (1 + exp(-preds))
  grad <- preds - labels
  hess <- preds * (1 - preds)
  return(list(grad = grad, hess = hess))
}

xgb_bin_data <- xgboost::xgb.DMatrix(as.matrix(mtcars[, -9]), label = mtcars$am)
xgb_bin_fit <- xgboost::xgb.train(
  params = list(max_depth = 2, silent = 1, objective = "binary:logistic", base_score = 0.5),
  data = xgb_bin_data, nrounds = 50
)

xgb_reglinear <- xgboost::xgb.train(params = list(max_depth = 2, silent = 1, objective = "reg:squarederror", base_score = 0.5), data = xgb_bin_data, nrounds = 4)
xgb_binarylogitraw <- xgboost::xgb.train(params = list(max_depth = 2, silent = 1, objective = "binary:logitraw", base_score = 0.5), data = xgb_bin_data, nrounds = 4)
xgb_reglogistic <- xgboost::xgb.train(params = list(max_depth = 2, silent = 1, objective = "reg:logistic", base_score = 0.5), data = xgb_bin_data, nrounds = 4)
xgb_binarylogistic <- xgboost::xgb.train(params = list(max_depth = 2, silent = 1, objective = "binary:logistic", base_score = 0.5), data = xgb_bin_data, nrounds = 4)
xgb_custom <- xgboost::xgb.train(params = list(max_depth = 2, silent = 1, objective = logregobj, base_score = 0.5), data = xgb_bin_data, nrounds = 4)

test_that("Returns the correct type", {
  expect_is(parse_model(xgb_bin_fit), "list")
  expect_is(tidypredict_fit(xgb_bin_fit), "call")
})

test_that("Predictions are correct for different objectives", {
  expect_false(tidypredict_test(xgb_reglinear, df = mtcars, xg_df = xgb_bin_data)$alert)
  expect_false(tidypredict_test(xgb_binarylogitraw, df = mtcars, xg_df = xgb_bin_data)$alert)
  expect_false(tidypredict_test(xgb_reglogistic, df = mtcars, xg_df = xgb_bin_data)$alert)
  expect_false(tidypredict_test(xgb_binarylogistic, df = mtcars, xg_df = xgb_bin_data)$alert)
  expect_warning(tidypredict_fit(xgb_custom))
})

test_that("Confirm SQL function returns a query", {
  expect_equal(
    rlang::expr_text(tidypredict_sql(xgb_reglinear, dbplyr::simulate_odbc())[[1]]),
    "\"0.0 + CASE\\nWHEN ((`qsec` < 19.9549999 OR ((`qsec`) IS NULL)) AND (`wt` < 3.18000007 OR ((`wt`) IS NULL))) THEN (0.138461545)\\nWHEN (`qsec` >= 19.9549999 AND (`wt` < 3.18000007 OR ((`wt`) IS NULL))) THEN (-0.100000009)\\nWHEN ((`hp` < 290.0 OR ((`hp`) IS NULL)) AND `wt` >= 3.18000007) THEN (-0.141666666)\\nWHEN (`hp` >= 290.0 AND `wt` >= 3.18000007) THEN (0.075000003)\\nEND + CASE\\nWHEN ((`qsec` < 19.9549999 OR ((`qsec`) IS NULL)) AND (`wt` < 3.01250005 OR ((`wt`) IS NULL))) THEN (0.0994230807)\\nWHEN (`qsec` >= 19.9549999 AND (`wt` < 3.01250005 OR ((`wt`) IS NULL))) THEN (-0.0599999987)\\nWHEN ((`hp` < 254.5 OR ((`hp`) IS NULL)) AND `wt` >= 3.01250005) THEN (-0.102500007)\\nWHEN (`hp` >= 254.5 AND `wt` >= 3.01250005) THEN (0.0786538497)\\nEND + CASE\\nWHEN ((`gear` < 3.5 OR ((`gear`) IS NULL))) THEN (-0.0735312551)\\nWHEN ((`wt` < 3.01250005 OR ((`wt`) IS NULL)) AND `gear` >= 3.5) THEN (0.0720817298)\\nWHEN (`wt` >= 3.01250005 AND `gear` >= 3.5) THEN (-0.0186758228)\\nEND + CASE\\nWHEN ((`gear` < 3.5 OR ((`gear`) IS NULL))) THEN (-0.0528505854)\\nWHEN ((`qsec` < 19.9500008 OR ((`qsec`) IS NULL)) AND `gear` >= 3.5) THEN (0.0427994467)\\nWHEN (`qsec` >= 19.9500008 AND `gear` >= 3.5) THEN (-0.0515981652)\\nEND + 0.5\""
  )
})

context("xgboost-parsnip")
test_that("Predictions are correct for different objectives", {
  m <- parsnip::fit(parsnip::set_engine(parsnip::boost_tree(mode = "regression"), "xgboost"), am ~ ., data = mtcars)
  expect_false(tidypredict_test(m, df = mtcars)$alert)
})

context("xgboost-saved")
test_that("Model can be saved and re-loaded", {
  mp <- tempfile(fileext = ".yml")
  yaml::write_yaml(parse_model(xgb_reglinear), mp)
  l <- yaml::read_yaml(mp)
  pm <- as_parsed_model(l)
  expect_silent(tidypredict_fit(pm))
})

base_score <- mean(mtcars$am)
xgb_reglogistic_basescore <-
  xgboost::xgb.train(
    params = list(
      max_depth = 2,
      silent = 1,
      objective = "reg:logistic",
      base_score = base_score
    ),
    data = xgb_bin_data,
    nrounds = 4
  )
xgb_binarylogistic_basescore <-
  xgboost::xgb.train(
    params = list(
      max_depth = 2,
      silent = 1,
      objective = "binary:logistic",
      base_score = base_score
    ),
    data = xgb_bin_data,
    nrounds = 4
  )

testthat::test_that("Error expected when base score is equal to 0 or 1", {
  testthat::expect_error(
    xgb_reglogistic_basescore <-
      xgboost::xgb.train(
        params = list(
          max_depth = 2,
          silent = 1,
          objective = "reg:logistic",
          base_score = 1
        ),
        data = xgb_bin_data,
        nrounds = 4
      )
  )
  
  testthat::expect_error(
    xgb_binarylogistic_basescore <-
      xgboost::xgb.train(
        params = list(
          max_depth = 2,
          silent = 1,
          objective = "binary:logistic",
          base_score = 1
        ),
        data = xgb_bin_data,
        nrounds = 4
      )
  )
  testthat::expect_error(
    xgb_reglogistic_basescore <-
      xgboost::xgb.train(
        params = list(
          max_depth = 2,
          silent = 1,
          objective = "reg:logistic",
          base_score = 0
        ),
        data = xgb_bin_data,
        nrounds = 4
      )
  )
  
  testthat::expect_error(
    xgb_binarylogistic_basescore <-
      xgboost::xgb.train(
        params = list(
          max_depth = 2,
          silent = 1,
          objective = "binary:logistic",
          base_score = 0
        ),
        data = xgb_bin_data,
        nrounds = 4
      )
  )
})

parsedmodel_xgb_reglogistic_basescore <- parse_model(xgb_reglogistic_basescore)
parsedmodel_xgb_binarylogistic_basescore <- parse_model(xgb_binarylogistic_basescore)

testthat::test_that("Base scores is same", {
  expect_equal(base_score, parsedmodel_xgb_reglogistic_basescore$general$params$base_score)
  expect_equal(base_score, parsedmodel_xgb_binarylogistic_basescore$general$params$base_score)
})

xgb_reglogistic_basescore_sql <-
  build_fit_formula_xgb(parsedmodel_xgb_reglogistic_basescore)
xgb_binarylogistic_basescore_sql <-
  build_fit_formula_xgb(parsedmodel_xgb_binarylogistic_basescore)

xgb_reglogistic_basescore_pred_model <-
  predict(xgb_reglogistic_basescore, xgb_bin_data) %>% round(5)
xgb_binarylogistic_basescore_pred_model <-
  predict(xgb_binarylogistic_basescore, xgb_bin_data) %>% round(5)

xgb_reglogistic_basescore_pred_sql <-
  mtcars %>%
  dplyr::mutate_(yhat_sql = as.character(xgb_reglogistic_basescore_sql)[[3]]) %>%
  dplyr::mutate(yhat_sql = 1 - yhat_sql) %>%
  .$yhat_sql %>% 
  round(5)

xgb_binarylogistic_basescore_pred_sql <-
  mtcars %>%
  dplyr::mutate_(yhat_sql = as.character(xgb_binarylogistic_basescore_sql)[[3]]) %>%
  dplyr::mutate(yhat_sql = 1 - yhat_sql) %>%
  .$yhat_sql %>% 
  round(5)

testthat::test_that("Same predictions between model and parsed sql model", {
  expect_equal(xgb_reglogistic_basescore_pred_model, xgb_reglogistic_basescore_pred_sql)
  expect_equal(xgb_binarylogistic_basescore_pred_model, xgb_binarylogistic_basescore_pred_sql)
})
