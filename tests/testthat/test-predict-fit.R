test_that("tidypredict_fit.pm_tree works for v3 party", {
  skip_if_not_installed("partykit")
  model <- partykit::ctree(mpg ~ wt + cyl, data = mtcars)
  pm <- parse_model(model)
  fit <- tidypredict_fit(pm)
  expect_type(fit, "language")
})

test_that("tidypredict_fit.pm_tree works for v3 ranger", {
  skip_if_not_installed("ranger")
  model <- ranger::ranger(mpg ~ wt + cyl, data = mtcars, num.trees = 2)
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
