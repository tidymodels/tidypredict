skip_if_not_installed("xgboost")

logregobj <- function(preds, dtrain) {
  labels <- xgboost::getinfo(dtrain, "label")
  preds <- 1 / (1 + exp(-preds))
  grad <- preds - labels
  hess <- preds * (1 - preds)
  return(list(grad = grad, hess = hess))
}

xgb_bin_data <- xgboost::xgb.DMatrix(
  as.matrix(mtcars[, -9]),
  label = mtcars$am
)

xgb_list <- list(
  reg_sqr = list(objective = "reg:squarederror"),
  bin_log = list(objective = "binary:logitraw"),
  reg_log = list(objective = "reg:logistic"),
  bin_log = list(objective = "binary:logistic"),
  log_reg = list(objective = logregobj),
  reg_log_base = list(objective = "reg:logistic", base_score = mean(mtcars$am)),
  bin_log_base = list(objective = "binary:logistic", base_score = mean(mtcars$am))
) %>%
  purrr::map(~ {
    if (is.null(.x$base_score)) .x$base_score <- 0.5
    .x
  })

xgb_models_all <- xgb_list %>%
  purrr::imap(~ {
    xgboost::xgb.train(
      params = list(
        max_depth = 2, objective = .x$objective, base_score = .x$base_score
      ),
      data = xgb_bin_data,
      nrounds = 4
    )
  })

xgb_models <- xgb_models_all[names(xgb_models_all) != "log_reg"]

test_that("Predictions match to model's predict routine", {
  td_tests <- xgb_models %>%
    purrr::map(
      tidypredict_test,
      df = mtcars,
      xg_df = xgb_bin_data,
      threshold = 0.001
    )

  td_tests %>%
    purrr::imap(
      ~ {
        msg <- paste0("------ >> MODEL: ", .y)
        expect_false(.x$alert, info = msg)
      }
    )

  expect_warning(
    tidypredict_test(
      xgb_models_all$log_reg,
      df = mtcars,
      xg_df = xgb_bin_data
    )
  )
})

test_that("Confirm SQL function returns SQL query", {
  expect_snapshot(
    xgb_models_all %>%
      purrr::map(tidypredict_sql, dbplyr::simulate_odbc()) %>%
      purrr::map(rlang::expr_text)
  )
})

test_that("Base scores match", {
  xgb_scores <- xgb_list %>%
    purrr::map_dbl(~ .x$base_score)

  xgb_scores_pm <- xgb_models_all %>%
    purrr::map(parse_model) %>%
    purrr::map_dbl(~ .x$general$params$base_score)

  xgb_scores %>%
    seq_along() %>%
    purrr::map(
      ~ expect_equal(xgb_scores[.x], xgb_scores_pm[.x])
    )
})

test_that("Model can be saved and re-loaded", {
  mp <- tempfile(fileext = ".yml")
  yaml::write_yaml(parse_model(xgb_models$reg_sqr), mp)
  l <- yaml::read_yaml(mp)
  pm <- as_parsed_model(l)
  expect_snapshot(tidypredict_fit(pm))
})



test_that("Predictions are correct for different objectives", {
  m <- parsnip::fit(
    parsnip::set_engine(parsnip::boost_tree(mode = "regression"), "xgboost"),
    am ~ .,
    data = mtcars
  )

  expect_false(tidypredict_test(m, df = mtcars, threshold = 0.001)$alert)
})
