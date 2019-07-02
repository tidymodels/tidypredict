context("xgboost")

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

test_that("Returns the correct type", {
  expect_is(parse_model(xgb_bin_fit), "list")
  expect_is(tidypredict_fit(xgb_bin_fit), "call")
})

test_that("Predictions are correct for different objectives", {
  xgb_reglinear <- xgboost::xgb.train(params = list(max_depth = 2, silent = 1, objective = "reg:linear", base_score = 0.5), data = xgb_bin_data, nrounds = 4)
  xgb_binarylogitraw <- xgboost::xgb.train(params = list(max_depth = 2, silent = 1, objective = "binary:logitraw", base_score = 0.5), data = xgb_bin_data, nrounds = 4)
  xgb_reglogistic <- xgboost::xgb.train(params = list(max_depth = 2, silent = 1, objective = "reg:logistic", base_score = 0.5), data = xgb_bin_data, nrounds = 4)
  xgb_binarylogistic <- xgboost::xgb.train(params = list(max_depth = 2, silent = 1, objective = "binary:logistic", base_score = 0.5), data = xgb_bin_data, nrounds = 4)
  xgb_custom <- xgboost::xgb.train(params = list(max_depth = 2, silent = 1, objective = logregobj, base_score = 0.5), data = xgb_bin_data, nrounds = 4)

  b <- suppressWarnings(
    dplyr::mutate(mtcars,
                  pred_sql_reglinear = !!tidypredict_fit(xgb_reglinear),
                  pred_sql_binarylogitraw = !!tidypredict_fit(xgb_binarylogitraw),
                  pred_sql_reglogistic = !!tidypredict_fit(xgb_reglogistic),
                  pred_sql_binarylogistic = !!tidypredict_fit(xgb_binarylogistic),
                  pred_sql_custom = !!tidypredict_fit(xgb_custom),
                  pred_r_reglinear = predict(xgb_reglinear, xgb_bin_data),
                  pred_r_binarylogitraw = predict(xgb_binarylogitraw, xgb_bin_data),
                  pred_r_reglogistic = predict(xgb_reglogistic, xgb_bin_data),
                  pred_r_binarylogistic = predict(xgb_binarylogistic, xgb_bin_data),
                  pred_r_custom = predict(xgb_custom, xgb_bin_data)
                  )
    )

  expect_equal(b$pred_sql_reglinear, b$pred_r_reglinear, tol = 1e-6)
  expect_equal(b$pred_sql_binarylogitraw, b$pred_r_binarylogitraw, tol = 1e-6)
  expect_equal(b$pred_sql_reglogistic, b$pred_r_reglogistic, tol = 1e-6)
  expect_equal(b$pred_sql_binarylogistic, b$pred_r_binarylogistic, tol = 1e-6)
  expect_equal(b$pred_sql_custom, b$pred_r_custom, tol = 1e-6)

  expect_warning(tidypredict_fit(xgb_custom))

  expect_equal(ncol(tidypredict_to_column(b, xgb_binarylogistic)), ncol(b) + 1)
})
