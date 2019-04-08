library(xgboost)
library(testthat)
library(purrr)
library(dplyr)
library(magrittr)
library(RSQLite)
library(DBI)


logregobj <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  preds <- 1/(1 + exp(-preds))
  grad <- preds - labels
  hess <- preds * (1 - preds)
  return(list(grad = grad, hess = hess))
}

xgb_bin_data <- xgb.DMatrix(as.matrix(mtcars[,-9]), label = mtcars$am)
xgb_bin_fit <- xgb.train(params =list(max_depth = 2, silent = 1, objective = "binary:logistic", base_score = 0.5), data = xgb_bin_data, nrounds = 50)
xgb_dump_text_with_stats <- xgb.dump(xgb_bin_fit, dump_format = "text", with_stats = TRUE)
tidypredict_fit(xgb_bin_fit)
context("xgboost")


test_that("Returns the correct type", {
  expect_is(parse_model(xgb_bin_fit), "list")
  # expect_is(tidypredict_sql(xgb_bin_fit, dbplyr::simulate_dbi()), "sql")
  expect_is(tidypredict_fit(xgb_bin_fit), "call")
})

test_that("SQLite - Predictions are correct for different objectives", {
  
  # Create an ephemeral in-memory RSQLite database
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  
  dbWriteTable(con, "mtcars", mtcars)
  dbWriteTable(con, "mtcars2", mtcars)
  
  xgb_reglinear <- xgb.train(params =list(max_depth = 2, silent = 1, objective = "reg:linear", base_score = 0.5), data = xgb_bin_data, nrounds = 4)
  xgb_binarylogitraw <- xgb.train(params =list(max_depth = 2, silent = 1, objective = "binary:logitraw", base_score = 0.5), data = xgb_bin_data, nrounds = 4)
  xgb_reglogistic <- xgb.train(params =list(max_depth = 2, silent = 1, objective = "reg:logistic", base_score = 0.5), data = xgb_bin_data, nrounds = 4)
  xgb_binarylogistic <- xgb.train(params =list(max_depth = 2, silent = 1, objective = "binary:logistic", base_score = 0.5), data = xgb_bin_data, nrounds = 4)
  xgb_custom <- xgb.train(params =list(max_depth = 2, silent = 1, objective = logregobj, base_score = 0.5), data = xgb_bin_data, nrounds = 4)
  
  a <- suppressWarnings(tbl(con, "mtcars") %>% 
    mutate(
      pred_sql_reglinear = !!tidypredict_fit(xgb_reglinear),
      pred_sql_binarylogitraw = !!tidypredict_fit(xgb_binarylogitraw),
      pred_sql_reglogistic = !!tidypredict_fit(xgb_reglogistic),
      pred_sql_binarylogistic = !!tidypredict_fit(xgb_binarylogistic),
      pred_sql_custom = !!tidypredict_fit(xgb_custom)
    ))
  
  b <- collect(a) %>% 
    mutate(
      pred_r_reglinear = predict(xgb_reglinear, xgb_bin_data),
      pred_r_binarylogitraw = predict(xgb_binarylogitraw, xgb_bin_data),
      pred_r_reglogistic = predict(xgb_reglogistic, xgb_bin_data),
      pred_r_binarylogistic = predict(xgb_binarylogistic, xgb_bin_data),
      pred_r_custom = predict(xgb_custom, xgb_bin_data)
    )
  
  expect_equal(b$pred_sql_reglinear, b$pred_r_reglinear, tol = 1e-6)
  expect_equal(b$pred_sql_binarylogitraw, b$pred_sql_binarylogitraw, tol = 1e-6)
  expect_equal(b$pred_sql_reglogistic, b$pred_r_reglogistic, tol = 1e-6)
  expect_equal(b$pred_sql_binarylogistic, b$pred_r_binarylogistic, tol = 1e-6)
  expect_equal(b$pred_sql_custom, b$pred_r_custom, tol = 1e-6)
  
  expect_warning(tidypredict_fit(xgb_custom))
})
