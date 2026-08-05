bagger_reg <- function(times = 3) {
  set.seed(100)
  baguette::bagger(mpg ~ wt + cyl + disp, data = mtcars, times = times)
}

bagger_cls <- function(times = 3) {
  set.seed(100)
  baguette::bagger(Species ~ ., data = iris, times = times)
}

bagger_c50 <- function(times = 3) {
  set.seed(100)
  baguette::bagger(
    Species ~ .,
    data = iris,
    base_model = "C5.0",
    times = times
  )
}

test_that("returns the right output", {
  skip_if_not_installed("baguette")

  model <- bagger_reg()
  tf <- tidypredict_fit(model)
  pm <- parse_model(model)

  expect_type(tf, "language")
  expect_s3_class(pm, "list")
  expect_equal(length(pm), 2)
  expect_equal(pm$general$model, "bagger")
  expect_equal(pm$general$version, 3)
  expect_length(pm$tree_info_list, 3)

  expect_snapshot(rlang::expr_text(tf))
})

test_that("tidypredict_fit matches predict() - regression", {
  skip_if_not_installed("baguette")

  model <- bagger_reg()
  fit_pred <- dplyr::mutate(mtcars, pred = !!tidypredict_fit(model))$pred

  expect_equal(fit_pred, predict(model, mtcars)$.pred)
})

test_that("tidypredict_fit matches predict() - classification", {
  skip_if_not_installed("baguette")

  model <- bagger_cls()
  fit_pred <- dplyr::mutate(iris, pred = !!tidypredict_fit(model))$pred

  expect_equal(fit_pred, as.character(predict(model, iris)$.pred_class))
})

test_that("categorical predictors work correctly", {
  skip_if_not_installed("baguette")

  mtcars2 <- mtcars
  mtcars2$cyl <- factor(mtcars2$cyl)
  set.seed(100)
  model <- baguette::bagger(mpg ~ wt + cyl + disp, data = mtcars2, times = 3)

  fit_pred <- dplyr::mutate(mtcars2, pred = !!tidypredict_fit(model))$pred

  expect_equal(fit_pred, predict(model, mtcars2)$.pred)
})

test_that("times argument is respected", {
  skip_if_not_installed("baguette")

  model <- bagger_reg(times = 7)
  pm <- parse_model(model)

  expect_length(pm$tree_info_list, 7)
  expect_equal(
    dplyr::mutate(mtcars, pred = !!tidypredict_fit(model))$pred,
    predict(model, mtcars)$.pred
  )
})

test_that("rpart control arguments are respected", {
  skip_if_not_installed("baguette")

  set.seed(100)
  model <- baguette::bagger(
    mpg ~ wt + cyl + disp,
    data = mtcars,
    times = 3,
    cp = 0.001,
    minsplit = 5,
    maxdepth = 4
  )

  expect_type(tidypredict_fit(model), "language")
  expect_equal(
    dplyr::mutate(mtcars, pred = !!tidypredict_fit(model))$pred,
    predict(model, mtcars)$.pred
  )
})

test_that("produced case_when uses .default", {
  skip_if_not_installed("baguette")

  expect_match(rlang::expr_text(tidypredict_fit(bagger_reg())), "\\.default")
})

test_that("formulas produce correct predictions", {
  skip_if_not_installed("baguette")

  expect_snapshot(tidypredict_test(bagger_reg(), mtcars))
  expect_snapshot(tidypredict_test(bagger_cls(), iris))
})

test_that("tidypredict_test.bagger max_rows and alert work", {
  skip_if_not_installed("baguette")

  model <- bagger_reg()

  expect_equal(
    nrow(tidypredict_test(model, mtcars, max_rows = 10)$raw_results),
    10
  )

  result <- tidypredict_test(model, mtcars, threshold = -1)
  expect_true(result$alert)
  expect_match(result$message, "Fitted records above the threshold")
})

test_that("Model can be saved and re-loaded", {
  skip_if_not_installed("baguette")

  model <- bagger_reg()
  mp <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(parse_model(model), mp)
  pm <- as_parsed_model(yaml::read_yaml(mp))

  expect_equal(
    dplyr::mutate(mtcars, pred = !!tidypredict_fit(pm))$pred,
    predict(model, mtcars)$.pred,
    tolerance = 0.000001
  )
})

test_that("Classification model can be saved and re-loaded", {
  skip_if_not_installed("baguette")

  model <- bagger_cls()
  mp <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(parse_model(model), mp)
  pm <- as_parsed_model(yaml::read_yaml(mp))

  expect_equal(
    dplyr::mutate(iris, pred = !!tidypredict_fit(pm))$pred,
    as.character(predict(model, iris)$.pred_class)
  )
})

test_that("SQL translation works", {
  skip_if_not_installed("baguette")
  skip_if_not_installed("dbplyr")

  expect_s3_class(
    tidypredict_sql(bagger_reg(), dbplyr::simulate_dbi()),
    "sql"
  )
})

test_that("unsupported base models error", {
  skip_if_not_installed("baguette")
  skip_if_not_installed("earth")

  set.seed(100)
  model <- baguette::bagger(
    mpg ~ wt + cyl + disp,
    data = mtcars,
    base_model = "MARS",
    times = 2
  )

  expect_snapshot(error = TRUE, tidypredict_fit(model))
  expect_snapshot(error = TRUE, parse_model(model))
})

# Tests for .extract_bagger_trees() ---------------------------------------

test_that(".extract_bagger_trees returns one expression per tree", {
  skip_if_not_installed("baguette")

  model <- bagger_reg()
  trees <- .extract_bagger_trees(model)

  expect_type(trees, "list")
  expect_length(trees, 3)
  expect_true(all(vapply(trees, is.language, logical(1))))

  eval_env <- rlang::new_environment(
    data = as.list(mtcars),
    parent = asNamespace("dplyr")
  )
  preds <- vapply(
    trees,
    rlang::eval_tidy,
    numeric(nrow(mtcars)),
    env = eval_env
  )

  expect_equal(rowMeans(preds), predict(model, mtcars)$.pred)
})

test_that(".extract_bagger_trees errors on bad input", {
  skip_if_not_installed("baguette")

  expect_snapshot(error = TRUE, .extract_bagger_trees(list()))
  expect_snapshot(error = TRUE, .extract_bagger_trees(bagger_cls()))
})

# Tests for .extract_bagger_classprob() -----------------------------------

test_that(".extract_bagger_classprob returns one list per class", {
  skip_if_not_installed("baguette")

  model <- bagger_cls()
  res <- .extract_bagger_classprob(model)

  expect_type(res, "list")
  expect_named(res, levels(iris$Species))
  expect_true(all(vapply(res, length, integer(1)) == 3))
  expect_true(
    all(vapply(unlist(res, recursive = FALSE), is.language, logical(1)))
  )
})

test_that(".extract_bagger_classprob results match predict probabilities", {
  skip_if_not_installed("baguette")

  model <- bagger_cls()
  res <- .extract_bagger_classprob(model)

  eval_env <- rlang::new_environment(
    data = as.list(iris),
    parent = asNamespace("dplyr")
  )
  probs <- vapply(
    res,
    function(class_trees) {
      rowMeans(vapply(
        class_trees,
        rlang::eval_tidy,
        numeric(nrow(iris)),
        env = eval_env
      ))
    },
    numeric(nrow(iris))
  )

  expect_equal(rowSums(probs), rep(1, nrow(iris)))
  expect_equal(
    unname(probs),
    unname(as.matrix(predict(model, iris, type = "prob")))
  )
})

test_that(".extract_bagger_classprob errors on bad input", {
  skip_if_not_installed("baguette")

  expect_snapshot(error = TRUE, .extract_bagger_classprob(list()))
  expect_snapshot(error = TRUE, .extract_bagger_classprob(bagger_reg()))
})

test_that("binary classification works", {
  skip_if_not_installed("baguette")

  df <- mtcars
  df$am <- factor(df$am, labels = c("auto", "manual"))
  set.seed(100)
  model <- baguette::bagger(am ~ wt + cyl + disp, data = df, times = 3)

  expect_equal(
    dplyr::mutate(df, pred = !!tidypredict_fit(model))$pred,
    as.character(predict(model, df)$.pred_class)
  )
  expect_length(.extract_bagger_classprob(model), 2)
})

test_that("C5.0 base models return the right output", {
  skip_if_not_installed("baguette")
  skip_if_not_installed("C50")

  model <- bagger_c50()
  tf <- tidypredict_fit(model)
  pm <- parse_model(model)

  expect_type(tf, "language")
  expect_s3_class(pm, "list")
  expect_equal(length(pm), 2)
  expect_equal(pm$general$model, "bagger")
  expect_equal(pm$general$version, 3)
  expect_equal(pm$general$classes, levels(iris$Species))
  expect_length(pm$tree_info_list, 3)

  expect_snapshot(tidypredict_test(model, iris))
})

test_that("tidypredict_fit matches predict() - C5.0 base model", {
  skip_if_not_installed("baguette")
  skip_if_not_installed("C50")

  model <- bagger_c50()

  expect_equal(
    dplyr::mutate(iris, pred = !!tidypredict_fit(model))$pred,
    as.character(predict(model, iris)$.pred_class)
  )

  model <- bagger_c50(times = 7)

  expect_length(parse_model(model)$tree_info_list, 7)
  expect_equal(
    dplyr::mutate(iris, pred = !!tidypredict_fit(model))$pred,
    as.character(predict(model, iris)$.pred_class)
  )
})

test_that("C5.0 base models work with categorical predictors", {
  skip_if_not_installed("baguette")
  skip_if_not_installed("C50")

  df <- mtcars
  df$am <- factor(df$am, labels = c("auto", "manual"))
  df$cyl <- factor(df$cyl)
  df$vs <- factor(df$vs)
  set.seed(100)
  model <- baguette::bagger(
    am ~ cyl + vs + gear + carb,
    data = df,
    base_model = "C5.0",
    times = 3
  )

  expect_equal(
    dplyr::mutate(df, pred = !!tidypredict_fit(model))$pred,
    as.character(predict(model, df)$.pred_class)
  )
  expect_length(.extract_bagger_classprob(model), 2)
})

test_that("C5.0 control arguments are respected", {
  skip_if_not_installed("baguette")
  skip_if_not_installed("C50")

  set.seed(100)
  model <- baguette::bagger(
    Species ~ .,
    data = iris,
    base_model = "C5.0",
    times = 3,
    minCases = 5,
    noGlobalPruning = TRUE
  )

  expect_type(tidypredict_fit(model), "language")
  expect_equal(
    dplyr::mutate(iris, pred = !!tidypredict_fit(model))$pred,
    as.character(predict(model, iris)$.pred_class)
  )
})

test_that("C5.0 model can be saved and re-loaded", {
  skip_if_not_installed("baguette")
  skip_if_not_installed("C50")

  model <- bagger_c50()
  mp <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(parse_model(model), mp)
  pm <- as_parsed_model(yaml::read_yaml(mp))

  expect_equal(
    dplyr::mutate(iris, pred = !!tidypredict_fit(pm))$pred,
    as.character(predict(model, iris)$.pred_class)
  )
})

test_that("C5.0 SQL translation works", {
  skip_if_not_installed("baguette")
  skip_if_not_installed("C50")
  skip_if_not_installed("dbplyr")

  expect_s3_class(
    tidypredict_sql(bagger_c50(), dbplyr::simulate_dbi()),
    "sql"
  )
})

test_that(".extract_bagger_classprob works with C5.0 base models", {
  skip_if_not_installed("baguette")
  skip_if_not_installed("C50")

  model <- bagger_c50()
  res <- .extract_bagger_classprob(model)

  expect_type(res, "list")
  expect_named(res, levels(iris$Species))
  expect_true(all(vapply(res, length, integer(1)) == 3))
  expect_true(
    all(vapply(unlist(res, recursive = FALSE), is.language, logical(1)))
  )

  eval_env <- rlang::new_environment(
    data = as.list(iris),
    parent = asNamespace("dplyr")
  )
  probs <- vapply(
    res,
    function(class_trees) {
      rowMeans(vapply(
        class_trees,
        rlang::eval_tidy,
        numeric(nrow(iris)),
        env = eval_env
      ))
    },
    numeric(nrow(iris))
  )

  expect_equal(rowSums(probs), rep(1, nrow(iris)))
  expect_equal(
    unname(probs),
    unname(as.matrix(predict(model, iris, type = "prob")))
  )
})

test_that("C5.0 base models with a cost matrix error", {
  skip_if_not_installed("baguette")
  skip_if_not_installed("C50")

  df <- mtcars
  df$am <- factor(df$am, labels = c("auto", "manual"))
  set.seed(100)
  model <- baguette::bagger(
    am ~ wt + disp,
    data = df,
    base_model = "C5.0",
    times = 2,
    cost = 2
  )

  expect_snapshot(error = TRUE, tidypredict_fit(model))
  expect_snapshot(error = TRUE, parse_model(model))
})

test_that("small ensembles work", {
  skip_if_not_installed("baguette")

  model <- bagger_reg(times = 2)

  expect_equal(
    dplyr::mutate(mtcars, pred = !!tidypredict_fit(model))$pred,
    predict(model, mtcars)$.pred
  )
})
