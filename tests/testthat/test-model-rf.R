test_that("returns the right output", {
  skip_if_not_installed("randomForest")
  set.seed(1234)

  model <- randomForest::randomForest(mpg ~ ., data = mtcars, ntree = 3)

  tf <- tidypredict_fit(model)
  pm <- parse_model(model)

  expect_type(tf, "language")

  expect_s3_class(pm, "list")
  expect_equal(length(pm), 2)
  expect_equal(length(pm$tree_info_list), 3)
  expect_equal(pm$general$model, "randomForest")
  expect_equal(pm$general$version, 3)

  expect_snapshot(
    rlang::expr_text(tf)
  )
})

test_that("model can be saved and re-loaded", {
  skip_if_not_installed("randomForest")
  set.seed(1234)

  model <- randomForest::randomForest(mpg ~ ., data = mtcars, ntree = 3)

  fit_expr <- tidypredict_fit(model)
  fit_pred <- dplyr::mutate(mtcars, pred = !!fit_expr)$pred
  original_pred <- predict(model, mtcars)

  expect_equal(fit_pred, as.vector(original_pred))
})

test_that("formulas produce correct predictions", {
  skip_if_not_installed("randomForest")
  set.seed(1234)

  # regression
  expect_false(
    tidypredict_test(
      randomForest::randomForest(mpg ~ ., data = mtcars, ntree = 3),
      mtcars,
    )$alert
  )
})

test_that("split operator uses <= for left child (#192)", {
  skip_if_not_installed("randomForest")
  set.seed(42)
  df <- data.frame(x = c(1, 2, 3, 4), y = c(10, 20, 100, 200))
  suppressWarnings(
    model <- randomForest::randomForest(
      y ~ x,
      data = df,
      ntree = 1,
      nodesize = 2,
      maxnodes = 3
    )
  )

  test_df <- data.frame(x = c(2.99, 3, 3.01))

  native <- as.numeric(predict(model, test_df))
  fit <- tidypredict_fit(model)
  tidy <- rlang::eval_tidy(fit, test_df)

  expect_equal(native, tidy)
})

test_that("produced case_when uses .default", {
  skip_if_not_installed("randomForest")
  set.seed(1234)

  model <- randomForest::randomForest(mpg ~ ., data = mtcars, ntree = 3)

  fit <- tidypredict_fit(model)
  fit_text <- rlang::expr_text(fit)

  expect_match(fit_text, "\\.default")
})

test_that("classification models error with clear message (#193)", {
  skip_if_not_installed("randomForest")
  set.seed(123)
  model <- randomForest::randomForest(
    Species ~ Sepal.Length + Sepal.Width,
    data = iris,
    ntree = 3
  )

  expect_snapshot(tidypredict_fit(model), error = TRUE)
})

test_that("parse_model errors on classification model", {
  skip_if_not_installed("randomForest")
  set.seed(123)
  model <- randomForest::randomForest(
    Species ~ Sepal.Length + Sepal.Width,
    data = iris,
    ntree = 3
  )

  expect_snapshot(parse_model(model), error = TRUE)
})

# v2 backwards compatibility tests ---------------------------------------------

test_that("v2 parsed randomForest model produces correct predictions", {
  pm <- readRDS(test_path("backwards-compat", "rf-v2-regression.rds"))

  expect_equal(pm$general$version, 2)
  expect_true(!is.null(pm$trees))

  fit <- tidypredict_fit(pm)
  expect_type(fit, "language")

  # Verify predictions match expected values
  pred <- rlang::eval_tidy(fit, mtcars)
  expect_type(pred, "double")
  expect_length(pred, nrow(mtcars))
})

test_that("v2 parsed classification model errors", {
  pm <- readRDS(test_path("backwards-compat", "rf-v2-classification.rds"))

  expect_equal(pm$general$version, 2)
  expect_true(is.character(pm$trees[[1]][[1]]$prediction))

  expect_snapshot(tidypredict_fit(pm), error = TRUE)
})

# Tests for tidypredict_class_trees()

test_that("tidypredict_class_trees returns correct structure", {
  skip_if_not_installed("randomForest")
  set.seed(123)
  model <- randomForest::randomForest(
    Species ~ Sepal.Length + Sepal.Width,
    data = iris,
    ntree = 3
  )

  result <- tidypredict_class_trees(model)

  expect_type(result, "list")
  expect_length(result, 3)
  expect_named(result, levels(iris$Species))
  # Each class should have ntree expressions
  expect_length(result[[1]], 3)
})

test_that("tidypredict_class_trees errors on non-randomForest model", {
  model <- lm(mpg ~ ., data = mtcars)

  expect_snapshot(error = TRUE, tidypredict_class_trees(model))
})

test_that("tidypredict_class_trees errors on regression model", {
  skip_if_not_installed("randomForest")
  set.seed(123)
  model <- randomForest::randomForest(mpg ~ ., data = mtcars, ntree = 3)

  expect_snapshot(error = TRUE, tidypredict_class_trees(model))
})

test_that("tidypredict_class_trees works with binary classification", {
  skip_if_not_installed("randomForest")
  set.seed(123)
  mtcars$vs <- factor(mtcars$vs)
  model <- randomForest::randomForest(
    vs ~ disp + hp,
    data = mtcars,
    ntree = 3
  )

  result <- tidypredict_class_trees(model)

  expect_type(result, "list")
  expect_length(result, 2)
  expect_named(result, c("0", "1"))
})

test_that("tidypredict_class_trees produces correct vote counts", {
  skip_if_not_installed("randomForest")
  set.seed(123)
  model <- randomForest::randomForest(
    Species ~ .,
    data = iris,
    ntree = 5
  )

  class_trees <- tidypredict_class_trees(model)
  n_trees <- model$ntree

  # Sum votes for each class
  vote_counts <- sapply(names(class_trees), function(cls) {
    trees <- class_trees[[cls]]
    tree_vals <- sapply(trees, function(e) {
      rlang::eval_tidy(e, iris)
    })
    if (is.matrix(tree_vals)) rowSums(tree_vals) else tree_vals
  })

  # Calculate probabilities
  probs <- vote_counts / n_trees

  # Compare to native predictions
  native <- predict(model, iris, type = "prob")

  expect_equal(unname(probs), unname(native), tolerance = 1e-10)
})

test_that("tidypredict_class_trees works with single tree", {
  skip_if_not_installed("randomForest")
  set.seed(123)
  model <- randomForest::randomForest(
    Species ~ .,
    data = iris,
    ntree = 1
  )

  result <- tidypredict_class_trees(model)

  expect_type(result, "list")
  expect_length(result, 3)
  # Each class should have 1 expression
  expect_length(result[[1]], 1)
})

# Tests for tidypredict_trees() (regression)

test_that("tidypredict_trees returns correct structure", {
  skip_if_not_installed("randomForest")
  set.seed(123)
  model <- randomForest::randomForest(
    mpg ~ cyl + disp + hp,
    data = mtcars,
    ntree = 5
  )

  result <- tidypredict_trees(model)

  expect_type(result, "list")
  expect_length(result, 5)
  expect_all_true(vapply(result, is.language, logical(1)))
})

test_that("tidypredict_trees errors on non-randomForest model", {
  model <- lm(mpg ~ ., data = mtcars)

  expect_snapshot(error = TRUE, tidypredict_trees(model))
})

test_that("tidypredict_trees errors on classification model", {
  skip_if_not_installed("randomForest")
  set.seed(123)
  model <- randomForest::randomForest(
    Species ~ Sepal.Length + Sepal.Width,
    data = iris,
    ntree = 3
  )

  expect_snapshot(error = TRUE, tidypredict_trees(model))
})

test_that("tidypredict_trees produces correct predictions when averaged", {
  skip_if_not_installed("randomForest")
  set.seed(123)
  model <- randomForest::randomForest(
    mpg ~ cyl + disp + hp,
    data = mtcars,
    ntree = 5
  )

  trees <- tidypredict_trees(model)
  n_trees <- length(trees)

  tree_preds <- sapply(trees, function(e) rlang::eval_tidy(e, mtcars))
  avg_pred <- rowMeans(tree_preds)

  native <- as.numeric(predict(model, mtcars))

  expect_equal(avg_pred, native)
})

# Tests for tidypredict_n_trees()

test_that("tidypredict_n_trees matches the forest size", {
  skip_if_not_installed("randomForest")
  set.seed(123)
  model <- randomForest::randomForest(
    mpg ~ cyl + disp + hp,
    data = mtcars,
    ntree = 5
  )

  expect_equal(tidypredict_n_trees(model), model$ntree)
  expect_equal(tidypredict_n_trees(model), length(tidypredict_trees(model)))
})

test_that("parsed models use the right split variable at every node (#232)", {
  skip_if_not_installed("randomForest")
  set.seed(123)
  model <- randomForest::randomForest(mpg ~ wt + cyl, data = mtcars, ntree = 5)

  # Leaves report a split variable of 0, and indexing the term labels with a 0
  # used to shorten the vector, misaligning every split name after the first
  # leaf. The direct builder was unaffected, so only the parsed path was wrong.
  pm <- as_parsed_model(parse_model(model))

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(pm), mtcars),
    unname(predict(model, mtcars))
  )
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(pm), mtcars),
    rlang::eval_tidy(tidypredict_fit(model), mtcars)
  )
})

test_that("parsed model leaf values carry no names", {
  skip_if_not_installed("randomForest")
  set.seed(123)
  model <- randomForest::randomForest(mpg ~ wt + cyl, data = mtcars, ntree = 2)

  info <- rf_tree_info_full(model, 1, names(model$forest$ncat))
  expect_null(names(info$prediction))
  expect_null(names(info$splitvarName))
})

test_that("a missing predictor gives NA, matching predict() (#294)", {
  skip_if_not_installed("randomForest")

  set.seed(1)
  model <- randomForest::randomForest(mpg ~ wt + cyl + disp, data = mtcars)

  df <- mtcars
  df$wt[1:5] <- NA_real_
  # A predictor the row's path may never consult still yields NA, because
  # `randomForest::predict()` returns NA for any incomplete row.
  df$disp[6:8] <- NA_real_

  fit <- rlang::eval_tidy(tidypredict_fit(model), df)
  base <- unname(predict(model, df))

  expect_equal(is.na(fit), is.na(base))
  expect_equal(fit[-(1:8)], base[-(1:8)])
  expect_false(tidypredict_test(model, df = df)$alert)
})

test_that("a stump in the forest matches predict() (#362)", {
  skip_if_not_installed("randomForest")

  set.seed(1)
  df <- data.frame(x1 = rnorm(120), x2 = runif(120))
  df$y <- 3
  model <- suppressWarnings(
    randomForest::randomForest(y ~ x1 + x2, data = df, ntree = 10)
  )
  expect_true(any(model$forest$ndbigtree == 1))

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), df),
    unname(predict(model, df))
  )
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(parse_model(model)), df),
    unname(predict(model, df))
  )
  expect_false(tidypredict_test(model, df = df)$alert)
})

test_that("a stump from a zero-variance predictor matches predict() (#362)", {
  skip_if_not_installed("randomForest")

  set.seed(1)
  df <- data.frame(x1 = rnorm(120), xconst = 1)
  df$y <- 2 * df$x1 + rnorm(120, sd = 0.3)
  model <- randomForest::randomForest(y ~ x1 + xconst, data = df, ntree = 10)
  expect_true(any(model$forest$ndbigtree == 1))

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), df),
    unname(predict(model, df))
  )
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(parse_model(model)), df),
    unname(predict(model, df))
  )
})

test_that("unordered factor splits match predict() (#282)", {
  skip_if_not_installed("randomForest")

  set.seed(1)
  df <- transform(mtcars, gear = factor(gear), carb = factor(carb))
  model <- randomForest::randomForest(mpg ~ wt + gear + carb, data = df)

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), df),
    unname(predict(model, df))
  )
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(parse_model(model)), df),
    unname(predict(model, df))
  )
})

test_that("ordered factor splits match predict() (#282)", {
  skip_if_not_installed("randomForest")

  set.seed(1)
  df <- transform(mtcars, gear = factor(gear, ordered = TRUE))
  model <- randomForest::randomForest(mpg ~ wt + gear, data = df)

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), df),
    unname(predict(model, df))
  )
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(parse_model(model)), df),
    unname(predict(model, df))
  )
})

test_that("a factor split reads level bits, not a threshold (#282)", {
  skip_if_not_installed("randomForest")

  # A split point of 10 is binary 1010, so the 2nd and 4th levels go left
  split <- rf_split_info("f", 10, 4, c("a", "b", "c", "d"))

  expect_true(split$is_categorical)
  expect_equal(unlist(split$vals), c("b", "d"))
})

test_that("factor splits match predict() for class probabilities (#282)", {
  skip_if_not_installed("randomForest")

  set.seed(1)
  df <- transform(
    mtcars,
    gear = factor(gear),
    am = factor(am, labels = c("auto", "manual"))
  )
  model <- randomForest::randomForest(am ~ wt + gear, data = df)

  trees <- tidypredict_class_trees(model)
  probs <- sapply(trees, function(exprs) {
    rowMeans(sapply(exprs, \(e) rlang::eval_tidy(e, df)))
  })

  base <- unclass(predict(model, df, type = "prob"))[, colnames(probs)]
  expect_equal(probs, unname(base), ignore_attr = "dimnames")
})

test_that("awkward factor level names match predict() (#282)", {
  skip_if_not_installed("randomForest")

  df <- mtcars
  df$fac <- factor(c("a:b", "c d", "e", "a:b")[(seq_len(32) %% 4) + 1])
  df$unused <- factor(as.character(df$gear), levels = c("3", "4", "5", "9"))
  # `grp` is a prefix of `grphi`, which a parser matching names by prefix would
  # confuse.
  df$grp <- factor(ifelse(df$hp > 120, "hi", "lo"))
  df$grphi <- factor(ifelse(df$wt > 3, "x", "lo"))

  set.seed(1)
  model <- randomForest::randomForest(
    mpg ~ wt + fac + unused + grp + grphi,
    data = df,
    ntree = 20
  )

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), df),
    unname(predict(model, df))
  )
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(parse_model(model)), df),
    unname(predict(model, df))
  )
})

test_that("values sitting exactly on a split point match predict()", {
  skip_if_not_installed("randomForest")

  set.seed(1)
  model <- randomForest::randomForest(mpg ~ wt + hp, data = mtcars, ntree = 20)

  splits <- sort(unique(model$forest$xbestsplit[model$forest$xbestsplit != 0]))
  nd <- mtcars[rep(1, length(splits)), ]
  nd$wt <- splits

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), nd),
    unname(predict(model, nd))
  )
})

test_that("degenerate forests match predict()", {
  skip_if_not_installed("randomForest")

  flat <- transform(mtcars, mpg = 5)
  set.seed(1)
  stumps <- suppressWarnings(
    randomForest::randomForest(mpg ~ wt + hp, data = flat, ntree = 10)
  )
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(stumps), flat),
    unname(predict(stumps, flat))
  )

  set.seed(1)
  single <- randomForest::randomForest(mpg ~ wt, data = mtcars, ntree = 10)
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(single), mtcars),
    unname(predict(single, mtcars))
  )

  set.seed(1)
  capped <- randomForest::randomForest(
    mpg ~ wt + hp,
    data = mtcars,
    ntree = 10,
    maxnodes = 2
  )
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(capped), mtcars),
    unname(predict(capped, mtcars))
  )
})

test_that("corr.bias = TRUE matches predict()", {
  skip_if_not_installed("randomForest")

  set.seed(1)
  model <- randomForest::randomForest(
    mpg ~ wt + hp + disp,
    data = mtcars,
    ntree = 20,
    corr.bias = TRUE
  )

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), mtcars),
    unname(predict(model, mtcars))
  )
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(parse_model(model)), mtcars),
    unname(predict(model, mtcars))
  )
})
