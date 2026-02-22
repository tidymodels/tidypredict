test_that("returns the right output", {
  model <- partykit::ctree(mpg ~ am + cyl, data = mtcars)
  tf <- tidypredict_fit(model)
  pm <- parse_model(model)

  expect_type(tf, "language")

  expect_s3_class(pm, "list")
  expect_equal(length(pm), 2)
  expect_equal(pm$general$model, "party")
  expect_equal(pm$general$version, 3)

  expect_snapshot(
    rlang::expr_text(tf)
  )
})

test_that("tidypredict_fit produces correct predictions", {
  model <- partykit::ctree(mpg ~ am + cyl, data = mtcars)

  fit_expr <- tidypredict_fit(model)
  fit_pred <- dplyr::mutate(mtcars, pred = !!fit_expr)$pred
  original_pred <- as.vector(predict(model, mtcars))

  expect_equal(fit_pred, original_pred)
})

test_that("formulas produces correct predictions", {
  mtcars <- mtcars
  mtcars$am1 <- mtcars$am
  mtcars$am <- ifelse(mtcars$am == 1, "auto", "man")
  mtcars$am <- as.factor(mtcars$am)

  mtcars$cyl <- ifelse(mtcars$cyl == 4, "four", mtcars$cyl)
  mtcars$cyl <- ifelse(mtcars$cyl == 6, "six", mtcars$cyl)
  mtcars$cyl <- ifelse(mtcars$cyl == 8, "eight", mtcars$cyl)
  mtcars$cyl <- as.factor(mtcars$cyl)

  # normal
  expect_snapshot(
    tidypredict_test(
      partykit::ctree(mpg ~ am + cyl, data = mtcars),
      mtcars
    )
  )

  # offset
  expect_snapshot(
    tidypredict_test(
      partykit::ctree(mpg ~ wt, offset = am1, data = mtcars),
      mtcars
    )
  )

  # interaction
  expect_snapshot(
    tidypredict_test(
      partykit::ctree(mpg ~ wt + disp * cyl, data = mtcars),
      mtcars
    )
  )

  # interactions
  expect_snapshot(
    tidypredict_test(
      partykit::ctree(mpg ~ (wt + disp) * cyl, data = mtcars),
      mtcars
    )
  )
})

# .extract_partykit_classprob tests ------------------------------------------

test_that(".extract_partykit_classprob returns list of expressions", {
  model <- partykit::ctree(Species ~ Sepal.Length + Sepal.Width, data = iris)

  exprs <- .extract_partykit_classprob(model)

  expect_type(exprs, "list")
  expect_length(exprs, 3)
  expect_true(all(vapply(exprs, typeof, character(1)) == "language"))
})

test_that(".extract_partykit_classprob results match predict probabilities", {
  model <- partykit::ctree(Species ~ Sepal.Length + Sepal.Width, data = iris)

  exprs <- .extract_partykit_classprob(model)
  eval_env <- rlang::new_environment(
    data = as.list(iris),
    parent = asNamespace("dplyr")
  )
  probs <- lapply(exprs, rlang::eval_tidy, env = eval_env)
  combined <- do.call(cbind, probs)

  native <- predict(model, type = "prob")

  expect_equal(unname(combined), unname(native))
})

test_that(".extract_partykit_classprob errors on non-party model", {
  expect_snapshot(.extract_partykit_classprob(list()), error = TRUE)
})

test_that("stump trees (no splits) work correctly (#196)", {
  ctrl <- partykit::ctree_control(mincriterion = 0.9999999)
  model <- partykit::ctree(mpg ~ cyl + disp + hp, data = mtcars, control = ctrl)

  # Verify it's a stump (only root node, no splits)
  expect_equal(length(partykit::nodeids(model, terminal = TRUE)), 1)

  fit <- tidypredict_fit(model)

  expect_type(fit, "double")
  expect_equal(fit, mean(mtcars$mpg))
})

test_that("produced case_when uses .default", {
  model <- partykit::ctree(mpg ~ am + cyl, data = mtcars)

  fit <- tidypredict_fit(model)
  fit_text <- rlang::expr_text(fit)

  expect_match(fit_text, "\\.default")
})

# Nested case_when tests --------------------------------------------------

test_that("tidypredict_fit matches original model predictions", {
  model <- partykit::ctree(mpg ~ cyl + wt, data = mtcars)

  fit_expr <- tidypredict_fit(model)
  fit_pred <- dplyr::mutate(mtcars, pred = !!fit_expr)$pred
  original_pred <- predict(model, mtcars)

  expect_equal(fit_pred, as.vector(original_pred))
})

test_that("tidypredict_fit works for classification", {
  model <- partykit::ctree(Species ~ ., data = iris)

  fit_expr <- tidypredict_fit(model)
  fit_pred <- dplyr::mutate(iris, pred = !!fit_expr)$pred
  original_pred <- as.character(predict(model, iris, type = "response"))

  expect_equal(fit_pred, original_pred)
})

test_that(".extract_partykit_classprob matches original model probabilities", {
  model <- partykit::ctree(Species ~ Sepal.Length + Sepal.Width, data = iris)

  exprs <- .extract_partykit_classprob(model)

  eval_env <- rlang::new_environment(
    data = as.list(iris),
    parent = asNamespace("dplyr")
  )

  probs <- lapply(exprs, rlang::eval_tidy, env = eval_env)
  combined <- do.call(cbind, probs)
  native <- predict(model, type = "prob")

  expect_equal(unname(combined), unname(native))
})

test_that(".partykit_tree_info_full is exported and works", {
  model <- partykit::ctree(mpg ~ cyl + wt, data = mtcars)

  tree_info <- .partykit_tree_info_full(model)

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
      "use_surrogates"
    )
  )
})
