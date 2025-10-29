test_that("get_rf_case() avoids ifelse if path is always TRUE (#143)", {
  path <- list()
  prediction <- list(
    list(col = "(Intercept)", val = 37.2, op = "none", is_intercept = 1),
    list(col = "hp", val = -0.0318, op = "multiply", is_intercept = 0),
    list(col = "wt", val = -3.88, op = "multiply", is_intercept = 0)
  )

  expect_identical(
    expr_text(get_rf_case(path, prediction, calc_mode = "ifelse")),
    "37.2 + hp * -0.0318 + wt * -3.88"
  )

  expect_identical(
    expr_text(get_rf_case(path, prediction, calc_mode = "")),
    "37.2 + hp * -0.0318 + wt * -3.88"
  )
})
