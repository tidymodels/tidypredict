# Version 3 is the current parsed model format. These fixtures pin it against
# the predictions the modelling package produced when they were generated, so
# a change that breaks reading an already-serialized v3 model is caught even
# though the format is current. See `backwards-compat/generate-v3-fixtures.R`.
#
# The expected values are stored, so these tests do not need the modelling
# packages installed.

expect_v3_fixture <- function(name, tolerance = 1e-12) {
  fixture <- readRDS(test_path("backwards-compat", paste0(name, ".rds")))
  pm <- as_parsed_model(fixture$pm)

  actual <- rlang::eval_tidy(tidypredict_fit(pm), fixture$newdata)
  if (is.character(fixture$expected)) {
    actual <- as.character(actual)
    testthat::expect_equal(actual, fixture$expected)
  } else {
    testthat::expect_equal(actual, fixture$expected, tolerance = tolerance)
  }
  invisible(pm)
}

test_that("v3 rpart parsed models still predict correctly", {
  pm <- expect_v3_fixture("v3-rpart-regression")
  expect_equal(pm$general$version, 3)
})

test_that("v3 partykit parsed models still predict correctly", {
  expect_v3_fixture("v3-partykit-regression")
})

test_that("v3 randomForest parsed models still predict correctly", {
  expect_v3_fixture("v3-rf-regression")
})

test_that("v3 C5.0 parsed models still predict correctly", {
  expect_v3_fixture("v3-c50-classification")
})

test_that("v3 xgboost parsed models still predict correctly", {
  # `xgboost` stores its leaf values as float32, so the fixture's expected
  # values only carry about seven significant digits.
  expect_v3_fixture("v3-xgb-regression", tolerance = 1e-7)
})

test_that("v3 tree fixtures carry no dispatch class of their own", {
  # `tidypredict_fit.pm_tree()` derives the model's class at read time rather
  # than reading one off the object, precisely so that fixtures like these,
  # serialized without it, keep working.
  fixture <- readRDS(test_path("backwards-compat", "v3-rpart-regression.rds"))
  expect_false(any(grepl("^pm_tree_", class(fixture$pm))))
})
