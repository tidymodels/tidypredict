# Helper to create test model
make_catboost_model <- function() {
  set.seed(123)
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  y <- mtcars$hp

  pool <- catboost::catboost.load_pool(
    X,
    label = y,
    feature_names = as.list(c("mpg", "cyl", "disp"))
  )

  catboost::catboost.train(
    pool,
    params = list(
      iterations = 10L,
      depth = 3L,
      learning_rate = 0.5,
      loss_function = "RMSE",
      logging_level = "Silent",
      allow_writing_files = FALSE,
      train_dir = tempdir()
    )
  )
}

# Parser tests ---------------------------------------------------------------

test_that("parse_model returns correct structure", {
  skip_if_not_installed("catboost")
  model <- make_catboost_model()
  pm <- parse_model(model)

  expect_s3_class(pm, "parsed_model")
  expect_s3_class(pm, "pm_catboost")

  expect_equal(pm$general$model, "catboost.Model")
  expect_equal(pm$general$type, "catboost")
  expect_equal(pm$general$version, 1)

  expect_gt(length(pm$trees), 0)
})

test_that("correct number of trees extracted", {
  skip_if_not_installed("catboost")
  model <- make_catboost_model()
  pm <- parse_model(model)

  expect_equal(length(pm$trees), 10)
})

test_that("each tree has leaves with predictions and paths", {
  skip_if_not_installed("catboost")
  model <- make_catboost_model()
  pm <- parse_model(model)

  tree1 <- pm$trees[[1]]
  expect_gt(length(tree1), 0)

  leaf_names <- lapply(tree1, names)
  expect_true(all(vapply(
    leaf_names,
    \(x) all(c("prediction", "path") %in% x),
    logical(1)
  )))

  predictions <- vapply(tree1, \(x) x$prediction, double(1))
  expect_type(predictions, "double")

  paths <- lapply(tree1, \(x) x$path)
  expect_true(all(vapply(paths, is.list, logical(1))))
})

test_that("symmetric trees have 2^n_splits leaves", {
  skip_if_not_installed("catboost")

  model <- make_catboost_model()
  pm <- parse_model(model)

  n_leaves <- vapply(pm$trees, length, integer(1))
  n_splits <- vapply(pm$trees, \(tree) length(tree[[1]]$path), integer(1))

  expect_equal(n_leaves, 2L^n_splits)
})

test_that("path conditions have correct structure", {
  skip_if_not_installed("catboost")
  model <- make_catboost_model()
  pm <- parse_model(model)

  tree1 <- pm$trees[[1]]
  leaves_with_paths <- which(sapply(tree1, function(x) length(x$path) > 0))

  if (length(leaves_with_paths) > 0) {
    leaf_with_path <- tree1[[leaves_with_paths[1]]]

    cond <- leaf_with_path$path[[1]]
    expect_equal(cond$type, "conditional")
    expect_contains(names(cond), c("col", "val", "op", "missing"))
    expect_contains(c("less-equal", "more"), cond$op)
  }
})

test_that("feature names are extracted", {
  skip_if_not_installed("catboost")
  model <- make_catboost_model()
  pm <- parse_model(model)

  expect_equal(pm$general$feature_names, c("mpg", "cyl", "disp"))
})

test_that("params are extracted", {
  skip_if_not_installed("catboost")
  model <- make_catboost_model()
  pm <- parse_model(model)

  expect_contains(names(pm$general), "params")
  expect_equal(pm$general$params$objective, "RMSE")
})

test_that("niter and nfeatures are extracted", {
  skip_if_not_installed("catboost")
  model <- make_catboost_model()
  pm <- parse_model(model)

  expect_equal(pm$general$niter, 10)
  expect_equal(pm$general$nfeatures, 3)
})

test_that("path contains both more and less-equal operators", {
  skip_if_not_installed("catboost")
  model <- make_catboost_model()
  pm <- parse_model(model)

  # Collect all operators from first tree
  all_ops <- unlist(lapply(pm$trees[[1]], function(leaf) {
    sapply(leaf$path, function(p) p$op)
  }))

  expect_contains(all_ops, "more")
  expect_contains(all_ops, "less-equal")
})

test_that("leaf 0 has all less-equal conditions (binary 00...0)", {
  skip_if_not_installed("catboost")

  set.seed(42)
  X <- matrix(c(1, 1, 10, 10, 1, 10, 1, 10), ncol = 2)
  y <- c(100, 200, 300, 400)

  pool <- catboost::catboost.load_pool(
    X,
    label = y,
    feature_names = as.list(c("x1", "x2"))
  )
  model <- catboost::catboost.train(
    pool,
    params = list(
      iterations = 1L,
      depth = 2L,
      learning_rate = 1.0,
      loss_function = "RMSE",
      logging_level = "Silent",
      allow_writing_files = FALSE,
      train_dir = tempdir(),
      min_data_in_leaf = 1L
    )
  )

  pm <- parse_model(model)
  leaf_0 <- pm$trees[[1]][[1]]

  # Leaf 0 (binary 00) should have all less-equal operators
  ops <- vapply(leaf_0$path, \(p) p$op, character(1))
  expect_true(all(ops == "less-equal"))
})

test_that("last leaf has all more conditions (binary 11...1)", {
  skip_if_not_installed("catboost")

  set.seed(42)
  X <- matrix(c(1, 1, 10, 10, 1, 10, 1, 10), ncol = 2)
  y <- c(100, 200, 300, 400)

  pool <- catboost::catboost.load_pool(
    X,
    label = y,
    feature_names = as.list(c("x1", "x2"))
  )
  model <- catboost::catboost.train(
    pool,
    params = list(
      iterations = 1L,
      depth = 2L,
      learning_rate = 1.0,
      loss_function = "RMSE",
      logging_level = "Silent",
      allow_writing_files = FALSE,
      train_dir = tempdir(),
      min_data_in_leaf = 1L
    )
  )

  pm <- parse_model(model)
  n_leaves <- length(pm$trees[[1]])
  last_leaf <- pm$trees[[1]][[n_leaves]]

  # Last leaf (binary 11) should have all more operators
  ops <- vapply(last_leaf$path, \(p) p$op, character(1))
  expect_true(all(ops == "more"))
})

test_that("leaf index binary representation determines operator pattern", {
  skip_if_not_installed("catboost")

  set.seed(42)
  X <- matrix(c(1, 1, 10, 10, 1, 10, 1, 10), ncol = 2)
  y <- c(100, 200, 300, 400)

  pool <- catboost::catboost.load_pool(
    X,
    label = y,
    feature_names = as.list(c("x1", "x2"))
  )
  model <- catboost::catboost.train(
    pool,
    params = list(
      iterations = 1L,
      depth = 2L,
      learning_rate = 1.0,
      loss_function = "RMSE",
      logging_level = "Silent",
      allow_writing_files = FALSE,
      train_dir = tempdir(),
      min_data_in_leaf = 1L
    )
  )

  pm <- parse_model(model)
  tree <- pm$trees[[1]]

  # With 2 splits, we have 4 leaves (indices 0-3)
  # Leaf index binary -> operator pattern:
  # 0 (00): [<=, <=]
  # 1 (01): [>, <=]
  # 2 (10): [<=, >]
  # 3 (11): [>, >]

  for (leaf_idx in 0:3) {
    leaf <- tree[[leaf_idx + 1]]
    n_splits <- length(leaf$path)

    for (split_idx in seq_len(n_splits)) {
      bit_val <- bitwAnd(bitwShiftR(leaf_idx, split_idx - 1L), 1L)
      expected_op <- if (bit_val == 1L) "more" else "less-equal"
      expect_equal(
        leaf$path[[split_idx]]$op,
        expected_op,
        info = sprintf("leaf %d, split %d", leaf_idx, split_idx)
      )
    }
  }
})

test_that("single split tree produces correct paths", {
  skip_if_not_installed("catboost")

  set.seed(42)
  X <- matrix(c(1, 10), ncol = 1)
  y <- c(0, 100)

  pool <- catboost::catboost.load_pool(
    X,
    label = y,
    feature_names = as.list(c("x"))
  )
  model <- catboost::catboost.train(
    pool,
    params = list(
      iterations = 1L,
      depth = 1L,
      learning_rate = 1.0,
      loss_function = "RMSE",
      logging_level = "Silent",
      allow_writing_files = FALSE,
      train_dir = tempdir(),
      min_data_in_leaf = 1L
    )
  )

  pm <- parse_model(model)

  expect_length(pm$trees[[1]], 2) # 2^1 = 2 leaves

  # Leaf 0: x <= border
  expect_equal(pm$trees[[1]][[1]]$path[[1]]$op, "less-equal")
  expect_equal(pm$trees[[1]][[1]]$path[[1]]$col, "x")

  # Leaf 1: x > border
  expect_equal(pm$trees[[1]][[2]]$path[[1]]$op, "more")
  expect_equal(pm$trees[[1]][[2]]$path[[1]]$col, "x")
})

test_that("model without explicit feature names still works", {
  skip_if_not_installed("catboost")

  set.seed(789)
  X <- data.matrix(mtcars[, c("mpg", "cyl")])
  y <- mtcars$hp

  # Create pool WITHOUT specifying feature_names
  pool <- catboost::catboost.load_pool(X, label = y)

  model <- catboost::catboost.train(
    pool,
    params = list(
      iterations = 3L,
      depth = 2L,
      learning_rate = 1.0,
      loss_function = "RMSE",
      logging_level = "Silent",
      allow_writing_files = FALSE,
      train_dir = tempdir(),
      min_data_in_leaf = 1L
    )
  )

  pm <- parse_model(model)

  expect_s3_class(pm, "pm_catboost")
  expect_length(pm$trees, 3)
  expect_equal(pm$general$nfeatures, 2)
  expect_type(pm$general$feature_names, "character")
})

test_that("deeper tree paths are traced correctly", {
  skip_if_not_installed("catboost")

  # Create data that forces a 3-split tree
  set.seed(123)
  n <- 100
  X <- matrix(rnorm(n * 3), ncol = 3)
  colnames(X) <- c("a", "b", "c")
  y <- X[, 1] + X[, 2] * 2 + X[, 3] * 3 + rnorm(n, sd = 0.1)

  pool <- catboost::catboost.load_pool(
    X,
    label = y,
    feature_names = as.list(c("a", "b", "c"))
  )
  model <- catboost::catboost.train(
    pool,
    params = list(
      iterations = 1L,
      depth = 3L,
      learning_rate = 1.0,
      loss_function = "RMSE",
      logging_level = "Silent",
      allow_writing_files = FALSE,
      train_dir = tempdir(),
      min_data_in_leaf = 1L
    )
  )

  pm <- parse_model(model)
  tree <- pm$trees[[1]]

  # All leaves should have 3 conditions in their path (if tree is full depth)
  n_splits <- length(tree[[1]]$path)
  expect_lte(n_splits, 3)

  # Verify leaf count matches 2^n_splits
  expect_equal(length(tree), 2^n_splits)
})

# Fit formula tests -------------------------------------------------------

test_that("tidypredict_fit returns language object", {
  skip_if_not_installed("catboost")
  model <- make_catboost_model()

  result <- tidypredict_fit(model)

  expect_type(result, "language")
})

test_that("tidypredict_fit works on parsed model", {
  skip_if_not_installed("catboost")
  model <- make_catboost_model()
  pm <- parse_model(model)

  result <- tidypredict_fit(pm)

  expect_type(result, "language")
})

test_that("regression predictions match catboost.predict", {
  skip_if_not_installed("catboost")
  model <- make_catboost_model()

  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  pool <- catboost::catboost.load_pool(X)

  native_preds <- catboost::catboost.predict(model, pool)
  formula <- tidypredict_fit(model)
  tidy_preds <- rlang::eval_tidy(formula, mtcars)

  expect_equal(tidy_preds, native_preds, tolerance = 1e-10)
})

test_that("Logloss predictions match catboost.predict", {
  skip_if_not_installed("catboost")

  set.seed(456)
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  y <- as.numeric(mtcars$am)

  pool <- catboost::catboost.load_pool(
    X,
    label = y,
    feature_names = as.list(c("mpg", "cyl", "disp"))
  )

  model <- catboost::catboost.train(
    pool,
    params = list(
      iterations = 10L,
      depth = 3L,
      learning_rate = 0.5,
      loss_function = "Logloss",
      logging_level = "Silent",
      allow_writing_files = FALSE,
      train_dir = tempdir()
    )
  )

  native_preds <- catboost::catboost.predict(
    model,
    pool,
    prediction_type = "Probability"
  )
  formula <- tidypredict_fit(model)
  tidy_preds <- rlang::eval_tidy(formula, mtcars)

  expect_equal(tidy_preds, native_preds, tolerance = 1e-10)
})

test_that("MAE predictions match catboost.predict", {
  skip_if_not_installed("catboost")

  set.seed(123)
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  y <- mtcars$hp

  pool <- catboost::catboost.load_pool(
    X,
    label = y,
    feature_names = as.list(c("mpg", "cyl", "disp"))
  )

  model <- catboost::catboost.train(
    pool,
    params = list(
      iterations = 10L,
      depth = 3L,
      learning_rate = 0.5,
      loss_function = "MAE",
      logging_level = "Silent",
      allow_writing_files = FALSE,
      train_dir = tempdir()
    )
  )

  native_preds <- catboost::catboost.predict(model, pool)
  formula <- tidypredict_fit(model)
  tidy_preds <- rlang::eval_tidy(formula, mtcars)

  expect_equal(tidy_preds, native_preds, tolerance = 1e-10)
})

test_that("Quantile predictions match catboost.predict", {
  skip_if_not_installed("catboost")

  set.seed(123)
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  y <- mtcars$hp

  pool <- catboost::catboost.load_pool(
    X,
    label = y,
    feature_names = as.list(c("mpg", "cyl", "disp"))
  )

  model <- catboost::catboost.train(
    pool,
    params = list(
      iterations = 10L,
      depth = 3L,
      learning_rate = 0.5,
      loss_function = "Quantile:alpha=0.5",
      logging_level = "Silent",
      allow_writing_files = FALSE,
      train_dir = tempdir()
    )
  )

  native_preds <- catboost::catboost.predict(model, pool)
  formula <- tidypredict_fit(model)
  tidy_preds <- rlang::eval_tidy(formula, mtcars)

  expect_equal(tidy_preds, native_preds, tolerance = 1e-10)
})

test_that("MAPE predictions match catboost.predict", {
  skip_if_not_installed("catboost")

  set.seed(123)
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  y <- mtcars$hp

  pool <- catboost::catboost.load_pool(
    X,
    label = y,
    feature_names = as.list(c("mpg", "cyl", "disp"))
  )

  model <- catboost::catboost.train(
    pool,
    params = list(
      iterations = 10L,
      depth = 3L,
      learning_rate = 0.5,
      loss_function = "MAPE",
      logging_level = "Silent",
      allow_writing_files = FALSE,
      train_dir = tempdir()
    )
  )

  native_preds <- catboost::catboost.predict(model, pool)
  formula <- tidypredict_fit(model)
  tidy_preds <- rlang::eval_tidy(formula, mtcars)

  expect_equal(tidy_preds, native_preds, tolerance = 1e-10)
})

test_that("Poisson predictions match catboost.predict", {
  skip_if_not_installed("catboost")

  set.seed(123)
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  y <- mtcars$carb

  pool <- catboost::catboost.load_pool(
    X,
    label = y,
    feature_names = as.list(c("mpg", "cyl", "disp"))
  )

  model <- catboost::catboost.train(
    pool,
    params = list(
      iterations = 10L,
      depth = 3L,
      learning_rate = 0.5,
      loss_function = "Poisson",
      logging_level = "Silent",
      allow_writing_files = FALSE,
      train_dir = tempdir()
    )
  )

  native_preds <- catboost::catboost.predict(model, pool)
  formula <- tidypredict_fit(model)
  tidy_preds <- rlang::eval_tidy(formula, mtcars)

  expect_equal(tidy_preds, native_preds, tolerance = 1e-10)
})

test_that("CrossEntropy predictions match catboost.predict", {
  skip_if_not_installed("catboost")

  set.seed(456)
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  y <- as.numeric(mtcars$am)

  pool <- catboost::catboost.load_pool(
    X,
    label = y,
    feature_names = as.list(c("mpg", "cyl", "disp"))
  )

  model <- catboost::catboost.train(
    pool,
    params = list(
      iterations = 10L,
      depth = 3L,
      learning_rate = 0.5,
      loss_function = "CrossEntropy",
      logging_level = "Silent",
      allow_writing_files = FALSE,
      train_dir = tempdir()
    )
  )

  native_preds <- catboost::catboost.predict(
    model,
    pool,
    prediction_type = "Probability"
  )
  formula <- tidypredict_fit(model)
  tidy_preds <- rlang::eval_tidy(formula, mtcars)

  expect_equal(tidy_preds, native_preds, tolerance = 1e-10)
})

test_that("unsupported objective throws error", {
  skip_if_not_installed("catboost")

  pm <- list(
    general = list(
      params = list(objective = "MultiClass"),
      model = "catboost.Model",
      type = "catboost"
    ),
    trees = list(list(list(prediction = 1, path = list())))
  )
  class(pm) <- c("pm_catboost", "parsed_model", "list")

  expect_error(
    tidypredict_fit(pm),
    "Unsupported objective"
  )
})

test_that("empty trees throws error", {
  skip_if_not_installed("catboost")

  pm <- list(
    general = list(
      params = list(objective = "RMSE"),
      model = "catboost.Model",
      type = "catboost"
    ),
    trees = list()
  )
  class(pm) <- c("pm_catboost", "parsed_model", "list")

  expect_error(
    tidypredict_fit(pm),
    "Model has no trees"
  )
})

test_that("NULL objective defaults to RMSE (identity)", {
  skip_if_not_installed("catboost")

  pm <- list(
    general = list(
      params = list(), # No objective specified
      model = "catboost.Model",
      type = "catboost"
    ),
    trees = list(list(list(prediction = 5.0, path = list())))
  )
  class(pm) <- c("pm_catboost", "parsed_model", "list")

  result <- tidypredict_fit(pm)

  # Should be identity transformation (no sigmoid)
  expect_false(grepl("exp", deparse(result)))
})

test_that("stump tree (empty path) works", {
  skip_if_not_installed("catboost")

  pm <- list(
    general = list(
      params = list(objective = "RMSE"),
      model = "catboost.Model",
      type = "catboost"
    ),
    trees = list(list(list(prediction = 42.5, path = list())))
  )
  class(pm) <- c("pm_catboost", "parsed_model", "list")

  result <- tidypredict_fit(pm)
  value <- rlang::eval_tidy(result, data.frame(x = 1))

  expect_equal(value, 42.5)
})

test_that("scale and bias are extracted correctly", {
  skip_if_not_installed("catboost")
  model <- make_catboost_model()
  pm <- parse_model(model)

  expect_type(pm$general$scale, "integer")
  expect_type(pm$general$bias, "double")
})

# SQL generation tests ----------------------------------------------------

test_that("tidypredict_sql returns SQL class", {
  skip_if_not_installed("catboost")
  model <- make_catboost_model()

  sql <- tidypredict_sql(model, dbplyr::simulate_sqlite())

  expect_s3_class(sql, "sql")
})

test_that("SQL predictions match native predictions with SQLite", {
  skip_if_not_installed("catboost")
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  model <- make_catboost_model()

  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  pool <- catboost::catboost.load_pool(X)
  native_preds <- catboost::catboost.predict(model, pool)

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "mtcars", mtcars)

  sql <- tidypredict_sql(model, con)
  query <- paste0("SELECT ", sql, " AS pred FROM mtcars")
  sql_preds <- DBI::dbGetQuery(con, query)$pred

  expect_equal(sql_preds, native_preds, tolerance = 1e-10)
})

# Integration tests -------------------------------------------------------

test_that("tidypredict_test works for regression", {
  skip_if_not_installed("catboost")
  model <- make_catboost_model()
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])

  result <- tidypredict_test(model, xg_df = X)

  expect_s3_class(result, "tidypredict_test")
  expect_false(result$alert)
})

test_that("tidypredict_test works for binary classification", {
  skip_if_not_installed("catboost")

  set.seed(456)
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  y <- as.numeric(mtcars$am)

  pool <- catboost::catboost.load_pool(
    X,
    label = y,
    feature_names = as.list(c("mpg", "cyl", "disp"))
  )

  model <- catboost::catboost.train(
    pool,
    params = list(
      iterations = 10L,
      depth = 3L,
      learning_rate = 0.5,
      loss_function = "Logloss",
      logging_level = "Silent",
      allow_writing_files = FALSE,
      train_dir = tempdir()
    )
  )

  result <- tidypredict_test(model, xg_df = X)

  expect_s3_class(result, "tidypredict_test")
  expect_false(result$alert)
})

test_that("tidypredict_test requires matrix", {
  skip_if_not_installed("catboost")
  model <- make_catboost_model()

  expect_error(
    tidypredict_test(model),
    "require.*matrix"
  )
})

test_that(".extract_catboost_trees returns list of expressions", {
  skip_if_not_installed("catboost")
  model <- make_catboost_model()

  trees <- .extract_catboost_trees(model)

  expect_type(trees, "list")
  expect_length(trees, 10)
  expect_type(trees[[1]], "language")
})

test_that(".extract_catboost_trees errors on non-catboost model", {
  expect_error(
    .extract_catboost_trees(lm(mpg ~ wt, data = mtcars)),
    "catboost.Model"
  )
})

# YAML serialization tests ------------------------------------------------

test_that("parsed model can be saved and loaded via YAML", {
  skip_if_not_installed("catboost")
  skip_if_not_installed("yaml")

  model <- make_catboost_model()
  pm <- parse_model(model)

  tmp_file <- tempfile(fileext = ".yml")
  on.exit(unlink(tmp_file), add = TRUE)

  yaml::write_yaml(pm, tmp_file)
  loaded <- yaml::read_yaml(tmp_file)
  class(loaded) <- class(pm)

  expect_equal(loaded$general$model, pm$general$model)
  expect_equal(loaded$general$type, pm$general$type)
  expect_equal(loaded$general$niter, pm$general$niter)
})

test_that("loaded model produces same predictions", {
  skip_if_not_installed("catboost")
  skip_if_not_installed("yaml")

  model <- make_catboost_model()
  pm <- parse_model(model)

  tmp_file <- tempfile(fileext = ".yml")
  on.exit(unlink(tmp_file), add = TRUE)

  yaml::write_yaml(pm, tmp_file)
  loaded <- yaml::read_yaml(tmp_file)
  class(loaded) <- class(pm)

  original_preds <- rlang::eval_tidy(tidypredict_fit(pm), mtcars)
  loaded_preds <- rlang::eval_tidy(tidypredict_fit(loaded), mtcars)

  expect_equal(loaded_preds, original_preds, tolerance = 1e-6)
})
