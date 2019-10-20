context("cubist")
set.seed(100)
data("BostonHousing", package = "mlbench")
model <- Cubist::cubist(x = BostonHousing[, -14], y = BostonHousing$medv, committees = 3)
tf <- tidypredict_fit(model)
pm <- parse_model(model)

test_that("Returns the correct type and dimensions", {
  expect_is(pm, "list")
  expect_equal(length(pm), 2)
  expect_equal(length(pm$trees), 1)
  expect_equal(pm$general$model, "cubist")
  expect_equal(pm$general$version, 2)
})

test_that("Returns expected dplyr formula", {
  expect_equal(
    rlang::expr_text(tf),
    "(ifelse(nox >= 0.668, -1.11 + crim * -0.02 + nox * 21.4 + rm * \n    0.1 + age * -0.003 + dis * 2.93 + ptratio * -0.13 + b * 0.008 + \n    lstat * -0.33, 0) + ifelse(lstat >= 9.59 & nox < 0.668, 23.57 + \n    crim * 0.05 + nox * -5.2 + rm * 3.1 + age * -0.048 + dis * \n    -0.81 + rad * 0.02 + tax * -0.0041 + ptratio * -0.71 + b * \n    0.01 + lstat * -0.15, 0) + ifelse(lstat < 9.59 & rm < 6.226, \n    1.18 + crim * 3.83 + rm * 4.3 + age * -0.06 + dis * -0.09 + \n        tax * -0.003 + ptratio * -0.08 + lstat * -0.11, 0) + \n    ifelse(lstat < 9.59 & rm >= 6.226, -4.71 + crim * 2.22 + \n        zn * 0.008 + nox * -1.7 + rm * 9.2 + age * -0.04 + dis * \n        -0.71 + rad * 0.03 + tax * -0.0182 + ptratio * -0.72 + \n        lstat * -0.83, 0) + ifelse(dis < 1.755 & lstat >= 5.12, \n    122.32 + crim * -0.29 + nox * -21.6 + rm * -3 + dis * -30.88 + \n        rad * 0.02 + tax * -0.001 + b * -0.023 + lstat * -0.73, \n    0) + ifelse(rm < 6.545 & lstat >= 5.12, 27.8 + crim * -0.16 + \n    zn * 0.007 + nox * -3.9 + rm * 2 + age * -0.035 + dis * -0.7 + \n    rad * 0.28 + tax * -0.0135 + ptratio * -0.6 + b * 0.013 + \n    lstat * -0.25, 0) + ifelse(rm >= 6.545 & lstat >= 5.12, 22.21 + \n    crim * -0.04 + zn * 0.01 + indus * -0.02 + nox * -4 + rm * \n    4.7 + dis * -0.34 + rad * 0.11 + tax * -0.0248 + ptratio * \n    -0.9 + b * 0.002 + lstat * -0.1, 0) + ifelse(lstat < 5.12 & \n    rm < 8.034, -71.95 + rm * 17 + age * -0.06 + tax * -0.0112 + \n    ptratio * -0.48 + lstat * -0.03, 0) + ifelse(rm >= 8.034 & \n    dis >= 3.199, -32.79 + crim * -0.01 + zn * 0.005 + nox * \n    -1.8 + rm * 12.9 + age * -0.117 + dis * -0.15 + rad * 0.04 + \n    tax * -0.0246 + ptratio * -1.05 + lstat * -0.04, 0) + ifelse(lstat < \n    5.12 & dis < 3.199, 53.41 + rm * 1.6 + dis * -7.16 + tax * \n    0.0088 + lstat * -0.68, 0) + ifelse(nox >= 0.668, -36.31 + \n    crim * 0.08 + nox * 48.4 + dis * 7.52 + b * 0.01 + lstat * \n    -0.24, 0) + ifelse(lstat >= 9.53 & nox < 0.668, 28.04 + nox * \n    -4.8 + rm * 2.9 + age * -0.051 + dis * -0.86 + rad * 0.01 + \n    tax * -0.0019 + ptratio * -0.72 + lstat * -0.12, 0) + ifelse(lstat < \n    9.53, -26.05 + crim * 0.89 + nox * -2.3 + rm * 9.6 + dis * \n    -0.17 + rad * 0.02 + tax * -0.0055 + ptratio * -0.12 + b * \n    0.001 + lstat * -0.74, 0) + ifelse(lstat < 9.53 & dis < 2.64, \n    136.67 + crim * 7.2 + nox * -96.6 + rm * 1.1 + tax * -0.0033 + \n        ptratio * -3.31 + lstat * -0.1, 0))/3"
  )
})

context("cubist-saved")
test_that("Model can be saved and re-loaded", {
  mp <- tempfile(fileext = ".yml")
  yaml::write_yaml(pm, mp)
  l <- yaml::read_yaml(mp)
  pm <- as_parsed_model(l)
  expect_silent(tidypredict_fit(pm))
})
