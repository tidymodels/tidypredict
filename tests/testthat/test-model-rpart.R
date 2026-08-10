test_that("rpart_tree_info returns correct structure", {
  skip_if_not_installed("rpart")
  model <- rpart::rpart(mpg ~ cyl + wt, data = mtcars)
  tree_info <- rpart_tree_info(model)

  expect_s3_class(tree_info, "data.frame")
  expect_named(
    tree_info,
    c(
      "nodeID",
      "leftChild",
      "rightChild",
      "splitvarName",
      "splitval",
      "splitclass",
      "terminal",
      "prediction"
    )
  )
})

test_that("rpart_tree_info handles categorical predictors", {
  skip_if_not_installed("rpart")
  mtcars2 <- mtcars
  mtcars2$cyl <- factor(mtcars2$cyl)
  model <- rpart::rpart(mpg ~ cyl + wt, data = mtcars2)
  tree_info <- rpart_tree_info(model)

  # Should have splitclass values for categorical splits
  expect_true(any(!is.na(tree_info$splitclass)))
})

test_that("returns the right output", {
  skip_if_not_installed("rpart")
  model <- rpart::rpart(mpg ~ am + cyl, data = mtcars)
  tf <- tidypredict_fit(model)
  pm <- parse_model(model)

  expect_type(tf, "language")
  expect_s3_class(pm, "list")
  expect_equal(pm$general$model, "rpart")
  expect_equal(pm$general$version, 3)

  expect_snapshot(rlang::expr_text(tf))
})

test_that("tidypredict_fit produces correct predictions", {
  skip_if_not_installed("rpart")
  model <- rpart::rpart(mpg ~ am + cyl, data = mtcars)

  fit_expr <- tidypredict_fit(model)
  fit_pred <- dplyr::mutate(mtcars, pred = !!fit_expr)$pred
  original_pred <- predict(model, mtcars)

  expect_equal(fit_pred, unname(original_pred))
})

test_that("formulas produce correct predictions - regression", {
  skip_if_not_installed("rpart")
  expect_false(
    tidypredict_test(
      rpart::rpart(mpg ~ am + cyl + wt, data = mtcars),
      mtcars
    )$alert
  )
})

test_that("tidypredict_test.rpart max_rows parameter works", {
  skip_if_not_installed("rpart")
  model <- rpart::rpart(mpg ~ am + cyl + wt, data = mtcars)
  result <- tidypredict_test(model, mtcars, max_rows = 10)

  expect_equal(nrow(result$raw_results), 10)
})

test_that("tidypredict_test.rpart alert message works", {
  skip_if_not_installed("rpart")
  model <- rpart::rpart(mpg ~ am + cyl + wt, data = mtcars)

  # Use negative threshold to trigger alert
  result <- tidypredict_test(model, mtcars, threshold = -1)

  expect_true(result$alert)
  expect_match(result$message, "Fitted records above the threshold")
  expect_match(result$message, "Max difference")
})

test_that("formulas produce correct predictions - classification", {
  skip_if_not_installed("rpart")
  expect_false(
    tidypredict_test(
      rpart::rpart(Species ~ ., data = iris),
      iris
    )$alert
  )
})

test_that("categorical predictors work correctly", {
  skip_if_not_installed("rpart")
  mtcars2 <- mtcars
  mtcars2$cyl <- factor(mtcars2$cyl)

  expect_false(
    tidypredict_test(
      rpart::rpart(mpg ~ cyl + wt, data = mtcars2),
      mtcars2
    )$alert
  )
})

test_that("stump trees work correctly", {
  skip_if_not_installed("rpart")
  ctrl <- rpart::rpart.control(minsplit = 100, cp = 1)
  model <- rpart::rpart(mpg ~ cyl + disp, data = mtcars, control = ctrl)

  fit <- tidypredict_fit(model)

  expect_type(fit, "double")
  expect_equal(fit, mean(mtcars$mpg))
})

test_that("produced case_when uses .default", {
  skip_if_not_installed("rpart")
  model <- rpart::rpart(mpg ~ am + cyl, data = mtcars)

  fit <- tidypredict_fit(model)
  fit_text <- rlang::expr_text(fit)

  expect_match(fit_text, "\\.default")
})

# .extract_rpart_classprob tests ------------------------------------------

test_that(".extract_rpart_classprob returns list of expressions", {
  skip_if_not_installed("rpart")
  model <- rpart::rpart(Species ~ Sepal.Length + Sepal.Width, data = iris)

  exprs <- .extract_rpart_classprob(model)

  expect_type(exprs, "list")
  expect_length(exprs, 3)
  expect_true(all(vapply(exprs, typeof, character(1)) == "language"))
})

test_that(".extract_rpart_classprob results match predict probabilities", {
  skip_if_not_installed("rpart")
  model <- rpart::rpart(Species ~ Sepal.Length + Sepal.Width, data = iris)

  exprs <- .extract_rpart_classprob(model)
  eval_env <- rlang::new_environment(
    data = as.list(iris),
    parent = asNamespace("dplyr")
  )
  probs <- lapply(exprs, rlang::eval_tidy, env = eval_env)
  combined <- do.call(cbind, probs)

  native <- predict(model, type = "prob")

  expect_equal(unname(combined), unname(native))
})

test_that(".extract_rpart_classprob errors on non-rpart model", {
  skip_if_not_installed("rpart")
  expect_snapshot(.extract_rpart_classprob(list()), error = TRUE)
})

test_that(".extract_rpart_classprob errors on regression model", {
  skip_if_not_installed("rpart")
  model <- rpart::rpart(mpg ~ cyl + wt, data = mtcars)
  expect_snapshot(.extract_rpart_classprob(model), error = TRUE)
})

# Nested case_when tests --------------------------------------------------

test_that("tidypredict_fit matches original model predictions", {
  skip_if_not_installed("rpart")
  model <- rpart::rpart(mpg ~ cyl + wt, data = mtcars)

  fit_expr <- tidypredict_fit(model)
  fit_pred <- dplyr::mutate(mtcars, pred = !!fit_expr)$pred
  original_pred <- predict(model, mtcars)

  expect_equal(fit_pred, unname(original_pred))
})

test_that("tidypredict_fit works for classification", {
  skip_if_not_installed("rpart")
  model <- rpart::rpart(Species ~ ., data = iris)

  fit_expr <- tidypredict_fit(model)
  fit_pred <- dplyr::mutate(iris, pred = !!fit_expr)$pred
  original_pred <- as.character(predict(model, iris, type = "class"))

  expect_equal(fit_pred, original_pred)
})

test_that(".extract_rpart_classprob matches original model probabilities", {
  skip_if_not_installed("rpart")
  model <- rpart::rpart(Species ~ Sepal.Length + Sepal.Width, data = iris)

  exprs <- .extract_rpart_classprob(model)

  eval_env <- rlang::new_environment(
    data = as.list(iris),
    parent = asNamespace("dplyr")
  )

  probs <- lapply(exprs, rlang::eval_tidy, env = eval_env)
  combined <- do.call(cbind, probs)
  native <- predict(model, type = "prob")

  expect_equal(unname(combined), unname(native))
})

test_that(".rpart_tree_info_full is exported and works", {
  skip_if_not_installed("rpart")
  model <- rpart::rpart(mpg ~ cyl + wt, data = mtcars)

  tree_info <- .rpart_tree_info_full(model)

  expect_type(tree_info, "list")
  expect_named(
    tree_info,
    c(
      "nodeID",
      "leftChild",
      "rightChild",
      "splitvarName",
      "terminal",
      "prediction",
      "node_splits",
      "majority_left",
      "use_surrogates",
      "stops_at_node"
    )
  )
})

test_that("splits at an observed value use a strict inequality", {
  skip_if_not_installed("rpart")
  # `rpart` sends values strictly below the cut point to the left, which is
  # only observable when the cut point coincides with a value in the new data
  df <- data.frame(x = c(1, 1, 2, 2, 3, 3), y = c(0, 0, 0, 1, 1, 1))
  model <- rpart::rpart(
    y ~ x,
    data = df[df$x != 2, ],
    control = rpart::rpart.control(minsplit = 2, cp = 0)
  )

  expect_equal(
    dplyr::mutate(df, pred = !!tidypredict_fit(model))$pred,
    unname(predict(model, df))
  )
})

test_that("missing values route through surrogate splits (#294)", {
  skip_if_not_installed("rpart")

  set.seed(1)
  n <- 300
  df <- data.frame(x = rnorm(n), w = rnorm(n))
  # `z` is correlated with `x`, so it is chosen as a surrogate for it.
  df$z <- df$x * 0.9 + rnorm(n, 0, 0.3)
  df$y <- 2 * df$x - df$w + rnorm(n)

  new_df <- df
  set.seed(2)
  for (col in c("x", "z", "w")) {
    new_df[[col]][sample(n, 40)] <- NA_real_
  }

  model <- rpart::rpart(y ~ x + z + w, data = df)
  expect_gt(sum(model$frame$nsurrogate), 0)

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), new_df),
    unname(predict(model, new_df))
  )
})

test_that("every usesurrogate mode is followed (#294)", {
  skip_if_not_installed("rpart")

  set.seed(1)
  n <- 300
  df <- data.frame(x = rnorm(n), w = rnorm(n))
  df$z <- df$x * 0.9 + rnorm(n, 0, 0.3)
  df$y <- 2 * df$x - df$w + rnorm(n)

  new_df <- df
  set.seed(2)
  for (col in c("x", "z", "w")) {
    new_df[[col]][sample(n, 40)] <- NA_real_
  }
  # Rows missing every predictor exercise the fallback, which differs by mode:
  # 2 goes in the majority direction, 0 and 1 stop at the node.
  new_df[1:6, c("x", "z", "w")] <- NA_real_

  for (mode in 0:2) {
    model <- rpart::rpart(
      y ~ x + z + w,
      data = df,
      control = rpart::rpart.control(usesurrogate = mode)
    )
    expect_equal(
      rlang::eval_tidy(tidypredict_fit(model), new_df),
      unname(predict(model, new_df))
    )
  }
})

test_that("a tied split has no majority to go with (#294)", {
  skip_if_not_installed("rpart")

  # Both children hold 10 rows, so `rpart` stops at the node rather than
  # picking a side, and returns the node's own fitted value.
  df <- data.frame(x = c(1:10, 21:30), y = c(rep(0, 10), rep(1, 10)))
  model <- rpart::rpart(
    y ~ x,
    data = df,
    control = rpart::rpart.control(cp = 0)
  )

  new_df <- data.frame(x = NA_real_)
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), new_df),
    unname(predict(model, new_df))
  )
})
