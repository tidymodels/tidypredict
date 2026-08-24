test_that("returns the right output", {
  skip_if_not_installed("partykit")

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
  skip_if_not_installed("partykit")

  model <- partykit::ctree(mpg ~ am + cyl, data = mtcars)

  fit_expr <- tidypredict_fit(model)
  fit_pred <- dplyr::mutate(mtcars, pred = !!fit_expr)$pred
  original_pred <- as.vector(predict(model, mtcars))

  expect_equal(fit_pred, original_pred)
})

test_that("formulas produce correct predictions", {
  skip_if_not_installed("partykit")

  mtcars <- mtcars
  mtcars$am1 <- mtcars$am
  mtcars$am <- ifelse(mtcars$am == 1, "auto", "man")
  mtcars$am <- as.factor(mtcars$am)

  mtcars$cyl <- ifelse(mtcars$cyl == 4, "four", mtcars$cyl)
  mtcars$cyl <- ifelse(mtcars$cyl == 6, "six", mtcars$cyl)
  mtcars$cyl <- ifelse(mtcars$cyl == 8, "eight", mtcars$cyl)
  mtcars$cyl <- as.factor(mtcars$cyl)

  # normal
  expect_false(
    tidypredict_test(
      partykit::ctree(mpg ~ am + cyl, data = mtcars),
      mtcars
    )$alert
  )

  # offset
  expect_false(
    tidypredict_test(
      partykit::ctree(mpg ~ wt, offset = am1, data = mtcars),
      mtcars
    )$alert
  )

  # interaction
  expect_false(
    tidypredict_test(
      partykit::ctree(mpg ~ wt + disp * cyl, data = mtcars),
      mtcars
    )$alert
  )

  # interactions
  expect_false(
    tidypredict_test(
      partykit::ctree(mpg ~ (wt + disp) * cyl, data = mtcars),
      mtcars
    )$alert
  )
})

# tidypredict_class_exprs tests ----------------------------------------------

test_that("tidypredict_class_exprs returns list of expressions", {
  skip_if_not_installed("partykit")

  model <- partykit::ctree(Species ~ Sepal.Length + Sepal.Width, data = iris)

  exprs <- tidypredict_class_exprs(model)

  expect_type(exprs, "list")
  expect_length(exprs, 3)
  expect_named(exprs, levels(iris$Species))
  expect_all_equal(vapply(exprs, typeof, character(1)), "language")
})

test_that("tidypredict_class_exprs results match predict probabilities", {
  skip_if_not_installed("partykit")

  model <- partykit::ctree(Species ~ Sepal.Length + Sepal.Width, data = iris)

  exprs <- tidypredict_class_exprs(model)
  eval_env <- rlang::new_environment(
    data = as.list(iris),
    parent = asNamespace("dplyr")
  )
  probs <- lapply(exprs, rlang::eval_tidy, env = eval_env)
  combined <- do.call(cbind, probs)

  native <- predict(model, type = "prob")

  expect_equal(unname(combined), unname(native))
})

test_that("tidypredict_class_exprs errors on non-party model", {
  skip_if_not_installed("partykit")

  expect_snapshot(tidypredict_class_exprs(list()), error = TRUE)
})

test_that("stump trees (no splits) work correctly (#196)", {
  skip_if_not_installed("partykit")

  ctrl <- partykit::ctree_control(mincriterion = 0.9999999)
  model <- partykit::ctree(mpg ~ cyl + disp + hp, data = mtcars, control = ctrl)

  # Verify it's a stump (only root node, no splits)
  expect_equal(length(partykit::nodeids(model, terminal = TRUE)), 1)

  fit <- tidypredict_fit(model)

  expect_type(fit, "double")
  expect_equal(fit, mean(mtcars$mpg))
})

test_that("produced case_when uses .default", {
  skip_if_not_installed("partykit")

  model <- partykit::ctree(mpg ~ am + cyl, data = mtcars)

  fit <- tidypredict_fit(model)
  fit_text <- rlang::expr_text(fit)

  expect_match(fit_text, "\\.default")
})

# Nested case_when tests --------------------------------------------------

test_that("tidypredict_fit matches original model predictions", {
  skip_if_not_installed("partykit")

  model <- partykit::ctree(mpg ~ cyl + wt, data = mtcars)

  fit_expr <- tidypredict_fit(model)
  fit_pred <- dplyr::mutate(mtcars, pred = !!fit_expr)$pred
  original_pred <- predict(model, mtcars)

  expect_equal(fit_pred, as.vector(original_pred))
})

test_that("tidypredict_fit works for classification", {
  skip_if_not_installed("partykit")

  model <- partykit::ctree(Species ~ ., data = iris)

  fit_expr <- tidypredict_fit(model)
  fit_pred <- dplyr::mutate(iris, pred = !!fit_expr)$pred
  original_pred <- as.character(predict(model, iris, type = "response"))

  expect_equal(fit_pred, original_pred)
})

test_that("tidypredict_class_exprs matches original model probabilities", {
  skip_if_not_installed("partykit")

  model <- partykit::ctree(Species ~ Sepal.Length + Sepal.Width, data = iris)

  exprs <- tidypredict_class_exprs(model)

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
  skip_if_not_installed("partykit")

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
      "branches",
      "majority_left",
      "use_surrogates"
    )
  )
})

test_that("tidypredict_to_column() works", {
  skip_if_not_installed("partykit")
  model <- partykit::ctree(mpg ~ wt + cyl, data = mtcars)

  res <- tidypredict_to_column(mtcars, model)
  expect_equal(res$fit, unname(predict(model, mtcars)))
})

test_that("SQL translation works", {
  skip_if_not_installed("partykit")
  skip_if_not_installed("dbplyr")
  model <- partykit::ctree(mpg ~ wt + cyl, data = mtcars)

  sql <- tidypredict_sql(model, dbplyr::simulate_dbi())
  expect_s3_class(sql, "sql")
  expect_match(as.character(sql), "CASE")
})

test_that("model can be saved and re-loaded", {
  skip_if_not_installed("partykit")
  skip_if_not_installed("yaml")
  model <- partykit::ctree(mpg ~ wt + cyl, data = mtcars)

  tmp <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(parse_model(model), tmp)
  pm <- as_parsed_model(yaml::read_yaml(tmp))

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(pm), mtcars),
    unname(predict(model, mtcars)),
    tolerance = 1e-6
  )
})

test_that("tidypredict_test() agrees with predict()", {
  skip_if_not_installed("partykit")
  model <- partykit::ctree(mpg ~ wt + cyl, data = mtcars)

  expect_false(tidypredict_test(model, mtcars)$alert)
})

test_that("a missing predictor gives NA at the split that needs it (#294)", {
  skip_if_not_installed("partykit")

  set.seed(1)
  model <- partykit::ctree(mpg ~ wt + disp + cyl, data = mtcars)

  # `partykit` resolves a missing value by sampling the split probabilities
  # (see `partykit:::kidids_node`), so `predict()` returns a different answer
  # on each call. There is no value to match, so tidypredict returns `NA`.
  df <- mtcars
  df$wt[1:4] <- NA_real_
  fit <- rlang::eval_tidy(tidypredict_fit(model), df)

  expect_length(fit, nrow(df))
  expect_true(all(is.na(fit[1:4])))
  expect_equal(fit[-(1:4)], unname(predict(model, mtcars))[-(1:4)])
})

test_that("only the splits a row reaches propagate NA (#294)", {
  skip_if_not_installed("partykit")

  set.seed(1)
  model <- partykit::ctree(mpg ~ wt + disp + cyl, data = mtcars)

  # The fitted tree splits on `wt` and `disp` only, so blanking `cyl`
  # changes nothing.
  df <- mtcars
  df$cyl <- NA_real_
  fit <- rlang::eval_tidy(tidypredict_fit(model), df)

  expect_false(anyNA(fit))
  expect_equal(fit, unname(predict(model, mtcars)))
})

test_that("a `right = FALSE` split excludes the break point (#295)", {
  skip_if_not_installed("partykit")

  df <- data.frame(x = 1:6, y = c(10, 20, 30, 40, 50, 60))
  split <- partykit::partysplit(1L, breaks = 3, right = FALSE)
  node <- partykit::partynode(
    1L,
    split = split,
    kids = list(partykit::partynode(2L), partykit::partynode(3L))
  )
  model <- partykit::as.constparty(partykit::party(
    node,
    data = df,
    fitted = data.frame(
      "(fitted)" = partykit::fitted_node(node, df),
      "(response)" = df$y,
      "(weights)" = rep(1, 6),
      check.names = FALSE
    )
  ))

  base <- unname(predict(model, df))
  # `x == 3` belongs to the right branch, not the left
  expect_equal(rlang::eval_tidy(tidypredict_fit(model), df), base)
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(parse_model(model)), df),
    base
  )
})

test_that("ordered factor predictors match predict() (#295)", {
  skip_if_not_installed("partykit")

  set.seed(1)
  df <- transform(mtcars, gear = factor(gear, ordered = TRUE))
  model <- partykit::ctree(mpg ~ wt + gear, data = df)
  base <- unname(predict(model, df))

  expect_equal(rlang::eval_tidy(tidypredict_fit(model), df), base)
  expect_equal(rlang::eval_tidy(tidypredict_fit(parse_model(model)), df), base)
})

test_that("converted rpart trees are not branch-swapped (#295)", {
  skip_if_not_installed("partykit")
  skip_if_not_installed("rpart")

  # `as.party.rpart()` writes an `index` of `2, 1`, putting the interval below
  # the break on the second kid
  model <- partykit::as.party(rpart::rpart(mpg ~ wt + disp, data = mtcars))
  base <- unname(predict(model, mtcars))

  expect_equal(rlang::eval_tidy(tidypredict_fit(model), mtcars), base)
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(parse_model(model)), mtcars),
    base
  )
})

test_that("converted rpart trees with factors match predict() (#295)", {
  skip_if_not_installed("partykit")
  skip_if_not_installed("rpart")

  df <- transform(
    mtcars,
    gear = factor(gear, ordered = TRUE),
    carb = factor(carb)
  )
  model <- partykit::as.party(rpart::rpart(mpg ~ wt + gear + carb, data = df))
  base <- unname(predict(model, df))

  expect_equal(rlang::eval_tidy(tidypredict_fit(model), df), base)
  expect_equal(rlang::eval_tidy(tidypredict_fit(parse_model(model)), df), base)
})

test_that("multiway factor splits match predict() (#295)", {
  skip_if_not_installed("partykit")

  set.seed(1)
  df <- transform(mtcars, carb = factor(carb))
  model <- partykit::ctree(
    mpg ~ carb,
    data = df,
    control = partykit::ctree_control(multiway = TRUE, alpha = 0.9)
  )

  expect_gt(length(partykit::kids_node(partykit::node_party(model))), 2)

  base <- unname(predict(model, df))
  expect_equal(rlang::eval_tidy(tidypredict_fit(model), df), base)
  expect_equal(rlang::eval_tidy(tidypredict_fit(parse_model(model)), df), base)
})

test_that("multiway numeric splits match predict() (#295)", {
  skip_if_not_installed("partykit")

  df <- data.frame(x = c(-1, -0.5, -0.4, 0, 0.4, 0.5, 1))
  df$y <- seq_len(nrow(df)) * 10

  for (right in c(TRUE, FALSE)) {
    split <- partykit::partysplit(1L, breaks = c(-0.5, 0.5), right = right)
    node <- partykit::partynode(
      1L,
      split = split,
      kids = lapply(2:4, partykit::partynode)
    )
    model <- partykit::as.constparty(partykit::party(
      node,
      data = df,
      fitted = data.frame(
        "(fitted)" = partykit::fitted_node(node, df),
        "(response)" = df$y,
        "(weights)" = rep(1, nrow(df)),
        check.names = FALSE
      )
    ))

    base <- unname(predict(model, df))
    expect_equal(rlang::eval_tidy(tidypredict_fit(model), df), base)
    expect_equal(
      rlang::eval_tidy(tidypredict_fit(parse_model(model)), df),
      base
    )
  }
})

test_that("awkward factor level names match predict() (#295)", {
  skip_if_not_installed("partykit")

  # An unused level, a level holding a `:`, and a level whose name is also a
  # column in the data all break a parser that splits level names by hand.
  set.seed(1)
  df <- mtcars
  df$g <- factor(
    c("a:b", "wt", "c d")[df$cyl / 2 - 1],
    levels = c("a:b", "wt", "c d", "unused")
  )
  model <- partykit::ctree(mpg ~ g + wt, data = df)
  base <- unname(predict(model, df))

  expect_equal(rlang::eval_tidy(tidypredict_fit(model), df), base)
  expect_equal(rlang::eval_tidy(tidypredict_fit(parse_model(model)), df), base)
})

test_that("training data containing NA matches predict()", {
  skip_if_not_installed("partykit")

  set.seed(1)
  df <- mtcars
  df$wt[1:5] <- NA_real_
  model <- partykit::ctree(mpg ~ wt + disp + hp, data = df)

  # `predict()` on complete rows is deterministic even though the tree was
  # grown on data with holes in it.
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), mtcars),
    unname(predict(model, mtcars))
  )
})

test_that("an unused outcome level matches predict()", {
  skip_if_not_installed("partykit")

  set.seed(1)
  df <- iris[iris$Species != "virginica", ]
  model <- partykit::ctree(Species ~ Sepal.Length + Petal.Length, data = df)

  expect_equal(
    as.character(rlang::eval_tidy(tidypredict_fit(model), df)),
    as.character(predict(model, df, type = "response"))
  )

  exprs <- tidypredict_class_exprs(model)
  eval_env <- rlang::new_environment(
    data = as.list(df),
    parent = asNamespace("dplyr")
  )
  probs <- lapply(exprs, rlang::eval_tidy, env = eval_env)

  expect_equal(
    unname(do.call(cbind, probs)),
    unname(predict(model, df, type = "prob"))
  )
})

test_that("a constant outcome and single-row data match predict()", {
  skip_if_not_installed("partykit")

  set.seed(1)
  df <- transform(mtcars, const = 5)
  constant <- partykit::ctree(const ~ wt + cyl, data = df)
  expect_equal(tidypredict_fit(constant), 5)

  one_row <- partykit::ctree(mpg ~ wt, data = mtcars[1, ])
  expect_equal(tidypredict_fit(one_row), mtcars$mpg[1])
})

test_that("a multiway model can be saved and re-loaded (#295)", {
  skip_if_not_installed("partykit")
  skip_if_not_installed("yaml")

  set.seed(1)
  df <- transform(mtcars, carb = factor(carb))
  model <- partykit::ctree(
    mpg ~ carb,
    data = df,
    control = partykit::ctree_control(multiway = TRUE, alpha = 0.9)
  )

  tmp <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(parse_model(model), tmp)
  pm <- as_parsed_model(yaml::read_yaml(tmp))

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(pm), df),
    unname(predict(model, df)),
    tolerance = 1e-6
  )
})
