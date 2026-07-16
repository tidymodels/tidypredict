test_that("cforest regression predictions match", {
  skip_if_not_installed("partykit")

  set.seed(1)
  model <- partykit::cforest(mpg ~ wt + cyl, data = mtcars, ntree = 20)

  expect_type(tidypredict_fit(model), "language")
  expect_false(tidypredict_test(model, df = mtcars)$alert)
})

test_that("terminal nodes use in-bag weighted means, not unweighted means", {
  skip_if_not_installed("partykit")

  set.seed(1)
  model <- partykit::cforest(mpg ~ wt + cyl, data = mtcars, ntree = 20)

  y <- mtcars$mpg
  node_train <- predict(model, type = "node")
  node_new <- predict(model, newdata = mtcars, type = "node")

  weighted <- rep(0, nrow(mtcars))
  unweighted <- rep(0, nrow(mtcars))
  for (i in seq_along(model$nodes)) {
    w <- model$weights[[i]]
    wm <- tapply(w * y, node_train[[i]], sum) / tapply(w, node_train[[i]], sum)
    um <- tapply(y, node_train[[i]], mean)
    key <- as.character(node_new[[i]])
    weighted <- weighted + wm[key]
    unweighted <- unweighted + um[key]
  }
  weighted <- weighted / length(model$nodes)
  unweighted <- unweighted / length(model$nodes)

  fit <- rlang::eval_tidy(tidypredict_fit(model), mtcars)

  # tidypredict must match the in-bag weighted average, which is what predict()
  # returns, and must differ from the naive unweighted average.
  expect_equal(fit, as.numeric(weighted))
  expect_equal(
    fit,
    as.numeric(predict(model, newdata = mtcars, type = "response"))
  )
  expect_gt(max(abs(as.numeric(unweighted) - fit)), 1e-6)
})

test_that("cforest works with categorical predictors", {
  skip_if_not_installed("partykit")

  set.seed(1)
  model <- partykit::cforest(mpg ~ wt + factor(cyl), data = mtcars, ntree = 20)

  expect_false(tidypredict_test(model, df = mtcars)$alert)
})

test_that("cforest supports SQL", {
  skip_if_not_installed("partykit")

  set.seed(1)
  model <- partykit::cforest(mpg ~ wt + cyl, data = mtcars, ntree = 10)

  expect_s3_class(tidypredict_sql(model, dbplyr::simulate_dbi()), "sql")
})

test_that("parse_model roundtrips and produces correct predictions", {
  skip_if_not_installed("partykit")

  set.seed(1)
  model <- partykit::cforest(mpg ~ wt + cyl, data = mtcars, ntree = 10)

  pm <- parse_model(model)
  expect_s3_class(pm, "pm_tree")
  expect_equal(pm$general$model, "cforest")
  expect_identical(tidypredict_fit(pm), tidypredict_fit(model))

  base <- as.numeric(predict(model, newdata = mtcars, type = "response"))
  parsed <- rlang::eval_tidy(tidypredict_fit(pm), mtcars)
  expect_equal(parsed, base)
})

test_that("model can be saved and re-loaded", {
  skip_if_not_installed("partykit")
  skip_if_not_installed("yaml")

  set.seed(1)
  model <- partykit::cforest(mpg ~ wt + cyl, data = mtcars, ntree = 10)

  tmp <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(parse_model(model), tmp)
  reloaded <- as_parsed_model(yaml::read_yaml(tmp))

  base <- as.numeric(predict(model, newdata = mtcars, type = "response"))
  parsed <- rlang::eval_tidy(tidypredict_fit(reloaded), mtcars)
  expect_equal(parsed, base, tolerance = 1e-6)
})

test_that("classification errors with clear message", {
  skip_if_not_installed("partykit")

  set.seed(1)
  model <- partykit::cforest(Species ~ ., data = iris, ntree = 5)

  expect_snapshot(error = TRUE, tidypredict_fit(model))
  expect_snapshot(error = TRUE, parse_model(model))
})
