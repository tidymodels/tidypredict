test_that("returns the right output", {
  model <- partykit::ctree(mpg ~ am + cyl, data = mtcars)
  tf <- tidypredict_fit(model)
  pm <- parse_model(model)

  expect_type(tf, "language")

  expect_s3_class(pm, "list")
  expect_equal(length(pm), 2)
  expect_equal(pm$general$model, "party")
  expect_equal(pm$general$version, 2)

  expect_snapshot(
    rlang::expr_text(tf)
  )
})

test_that("Model can be saved and re-loaded", {
  model <- partykit::ctree(mpg ~ am + cyl, data = mtcars)

  pm <- parse_model(model)
  mp <- tempfile(fileext = ".yml")
  yaml::write_yaml(pm, mp)
  l <- yaml::read_yaml(mp)
  pm <- as_parsed_model(l)

  expect_identical(
    round_print(tidypredict_fit(model)),
    round_print(tidypredict_fit(pm))
  )
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

# Nested case_when tests --------------------------------------------------

test_that("tidypredict_fit with nested = TRUE matches flat predictions", {
  model <- partykit::ctree(mpg ~ cyl + wt, data = mtcars)

  flat_expr <- tidypredict_fit(model, nested = FALSE)
  nested_expr <- tidypredict_fit(model, nested = TRUE)

  flat_pred <- dplyr::mutate(mtcars, pred = !!flat_expr)$pred
  nested_pred <- dplyr::mutate(mtcars, pred = !!nested_expr)$pred

  expect_equal(nested_pred, flat_pred)
})

test_that("tidypredict_fit with nested = TRUE works for classification", {
  model <- partykit::ctree(Species ~ ., data = iris)

  flat_expr <- tidypredict_fit(model, nested = FALSE)
  nested_expr <- tidypredict_fit(model, nested = TRUE)

  flat_pred <- dplyr::mutate(iris, pred = !!flat_expr)$pred
  nested_pred <- dplyr::mutate(iris, pred = !!nested_expr)$pred

  expect_equal(nested_pred, flat_pred)
})

test_that(".extract_partykit_classprob with nested = TRUE matches flat", {
  model <- partykit::ctree(Species ~ Sepal.Length + Sepal.Width, data = iris)

  flat_exprs <- .extract_partykit_classprob(model, nested = FALSE)
  nested_exprs <- .extract_partykit_classprob(model, nested = TRUE)

  eval_env <- rlang::new_environment(
    data = as.list(iris),
    parent = asNamespace("dplyr")
  )

  flat_probs <- lapply(flat_exprs, rlang::eval_tidy, env = eval_env)
  nested_probs <- lapply(nested_exprs, rlang::eval_tidy, env = eval_env)

  expect_equal(nested_probs, flat_probs)
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
