skip_if_not_installed("xgboost")

logregobj <- function(preds, dtrain) {
  labels <- xgboost::getinfo(dtrain, "label")
  preds <- 1 / (1 + exp(-preds))
  grad <- preds - labels
  hess <- preds * (1 - preds)
  return(list(grad = grad, hess = hess))
}

xgb_bin_data <- xgboost::xgb.DMatrix(as.matrix(mtcars[, -9]), label = mtcars$am)

xgb_list <- list(
  reg_sqr = list(objective = "reg:squarederror"), 
  bin_log = list(objective = "binary:logitraw"), 
  reg_log = list(objective = "reg:logistic"), 
  bin_log = list(objective = "binary:logistic"), 
  log_reg = list(objective = logregobj),
  reg_log_base = list(objective = "reg:logistic", base_score = mean(mtcars$am)), 
  bin_log_base = list(objective = "binary:logistic", base_score = mean(mtcars$am))
  )

xgb_models <- xgb_list %>% 
  purrr::imap(~{
    if(is.null(.x$base_score)) .x$base_score <- 0.5
    xgboost::xgb.train(
      params = list(
        max_depth = 2, objective = .x$objective, base_score = .x$base_score
        ),
      data = xgb_bin_data, 
      nrounds = 4)
    }
  ) 
  
# xgb_bin_fit <- xgboost::xgb.train(
#   params = list(max_depth = 2, objective = "binary:logistic", base_score = 0.5),
#   data = xgb_bin_data, 
#   nrounds = 50
#   )
# 
# xgb_reglinear <- xgboost::xgb.train(
#   params = list(max_depth = 2, objective = "reg:squarederror", base_score = 0.5), 
#   data = xgb_bin_data, 
#   nrounds = 4
#   )
# 
# xgb_binarylogitraw <- xgboost::xgb.train(
#   params = list(max_depth = 2, objective = "binary:logitraw", base_score = 0.5), 
#   data = xgb_bin_data, 
#   nrounds = 4)
# 
# xgb_reglogistic <- xgboost::xgb.train(
#   params = list(max_depth = 2, objective = "reg:logistic", base_score = 0.5), 
#   data = xgb_bin_data, 
#   nrounds = 4
#   )
# 
# xgb_binarylogistic <- xgboost::xgb.train(
#   params = list(max_depth = 2, objective = "binary:logistic", base_score = 0.5), 
#   data = xgb_bin_data, 
#   nrounds = 4)
# 
# xgb_custom <- xgboost::xgb.train(
#   params = list(max_depth = 2, objective = logregobj, base_score = 0.5),
#   data = xgb_bin_data, 
#   nrounds = 4
#   )

# 
# base_score <- mean(mtcars$am)
# 
# xgb_reglogistic_basescore <-
#   xgboost::xgb.train(
#     params = list(
#       max_depth = 2,
#       objective = "reg:logistic",
#       base_score = base_score
#     ),
#     data = xgb_bin_data,
#     nrounds = 4
#   )
# 
# xgb_binarylogistic_basescore <-
#   xgboost::xgb.train(
#     params = list(
#       max_depth = 2,
#       objective = "binary:logistic",
#       base_score = base_score
#     ),
#     data = xgb_bin_data,
#     nrounds = 4
#   )

pm_xgb_reglogistic_basescore <- parse_model(xgb_reglogistic_basescore)

pm_xgb_binarylogistic_basescore <- parse_model(xgb_binarylogistic_basescore)

xgb_reglogistic_basescore_sql <- build_fit_formula_xgb(
  parsedmodel = pm_xgb_reglogistic_basescore
  )

xgb_binarylogistic_basescore_sql <-
  build_fit_formula_xgb(pm_xgb_binarylogistic_basescore)

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


test_that("Returns the correct type", {
  expect_s3_class(parse_model(xgb_bin_fit), "list")
  expect_equal(class(tidypredict_fit(xgb_bin_fit))[1], "call")
})

test_that("Predictions are correct for different objectives", {
  
  test_alert <- function(pm, df, xg_df) {
    tidypredict_test(pm, df = df, xg_df = xg_df)$alert
  }
  
  expect_false(test_alert(xgb_reglinear, mtcars, xgb_bin_data))
  expect_false(test_alert(xgb_binarylogitraw, mtcars, xgb_bin_data))
  expect_false(test_alert(xgb_reglogistic, mtcars, xgb_bin_data))
  expect_false(test_alert(xgb_binarylogistic, mtcars, xgb_bin_data))
  expect_warning(tidypredict_fit(xgb_custom))
})

test_that("Confirm SQL function returns a query", {
  expect_snapshot(
    rlang::expr_text(tidypredict_sql(xgb_reglinear, dbplyr::simulate_odbc())[[1]]),
  )
  
  expect_snapshot(
    rlang::expr_text(tidypredict_sql(xgb_reglogistic, dbplyr::simulate_odbc())[[1]])
  )
})

test_that("Predictions are correct for different objectives", {
  m <- parsnip::fit(
    parsnip::set_engine(parsnip::boost_tree(mode = "regression"), "xgboost"), 
    am ~ ., 
    data = mtcars)
  expect_false(tidypredict_test(m, df = mtcars)$alert)
})

test_that("Model can be saved and re-loaded", {
  mp <- tempfile(fileext = ".yml")
  yaml::write_yaml(parse_model(xgb_reglinear), mp)
  l <- yaml::read_yaml(mp)
  pm <- as_parsed_model(l)
  expect_snapshot(tidypredict_fit(pm))
})



testthat::test_that("Error expected when base score is equal to 0 or 1", {
  testthat::expect_error(
    xgb_reglogistic_basescore <-
      xgboost::xgb.train(
        params = list(
          max_depth = 2,
         
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
         
          objective = "binary:logistic",
          base_score = 0
        ),
        data = xgb_bin_data,
        nrounds = 4
      )
  )
})

testthat::test_that("Base scores is same", {
  expect_equal(
    base_score, 
    pm_xgb_reglogistic_basescore$general$params$base_score
  )
  expect_equal(
    base_score,
    pm_xgb_binarylogistic_basescore$general$params$base_score
  )
})

testthat::test_that("Same predictions between model and parsed sql model", {
  expect_equal(
    xgb_reglogistic_basescore_pred_model, 
    xgb_reglogistic_basescore_pred_sql)
  expect_equal(
    xgb_binarylogistic_basescore_pred_model,
    xgb_binarylogistic_basescore_pred_sql
    )
})
