test_that("generate_nested_case_when_tree works for regression", {
  skip_if_not_installed("rpart")

  model <- rpart::rpart(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)
  tree_info <- rpart_tree_info_full(model)
  nested_expr <- generate_nested_case_when_tree(tree_info)

  nested_pred <- dplyr::mutate(iris, pred = !!nested_expr)$pred
  original_pred <- predict(model, iris)

  expect_equal(nested_pred, unname(original_pred))
})

test_that("generate_nested_case_when_tree works for classification", {
  skip_if_not_installed("rpart")

  model <- rpart::rpart(
    Species ~ Sepal.Width + Petal.Length,
    data = iris,
    method = "class"
  )
  tree_info <- rpart_tree_info_full(model)
  nested_expr <- generate_nested_case_when_tree(tree_info)

  nested_pred <- dplyr::mutate(iris, pred = !!nested_expr)$pred
  original_pred <- as.character(predict(model, iris, type = "class"))

  expect_equal(nested_pred, original_pred)
})

test_that("generate_nested_case_when_tree handles categorical splits", {
  skip_if_not_installed("rpart")

  data <- mtcars
  data$cyl <- factor(data$cyl)
  model <- rpart::rpart(mpg ~ cyl + hp, data = data)
  tree_info <- rpart_tree_info_full(model)
  nested_expr <- generate_nested_case_when_tree(tree_info)

  nested_pred <- dplyr::mutate(data, pred = !!nested_expr)$pred
  original_pred <- predict(model, data)

  expect_equal(nested_pred, unname(original_pred))
})

test_that("generate_nested_case_when_tree handles stump trees", {
  skip_if_not_installed("rpart")

  model <- rpart::rpart(Sepal.Length ~ Sepal.Width, data = iris, cp = 1)
  tree_info <- rpart_tree_info_full(model)

  expect_equal(sum(tree_info$terminal), 1L)

  nested_expr <- generate_nested_case_when_tree(tree_info)

  expect_true(is.numeric(nested_expr))

  nested_pred <- dplyr::mutate(iris, pred = !!nested_expr)$pred
  original_pred <- predict(model, iris)

  expect_equal(nested_pred, unname(original_pred))
})

test_that("generate_nested_case_when_tree produces nested case_when structure", {
  skip_if_not_installed("rpart")

  model <- rpart::rpart(Sepal.Length ~ Petal.Length, data = iris)
  tree_info <- rpart_tree_info_full(model)
  nested_expr <- generate_nested_case_when_tree(tree_info)

  expr_str <- rlang::expr_deparse(nested_expr)
  expect_true(any(grepl("case_when", expr_str)))
  expect_true(any(grepl("\\.default", expr_str)))
})

test_that("build_nested_split_condition handles continuous splits", {
  split <- list(col = "x", val = 5, is_categorical = FALSE)
  cond <- build_nested_split_condition(split)

  expect_equal(rlang::expr_deparse(cond), "x <= 5")
})

test_that("build_nested_split_condition handles categorical splits", {
  split <- list(col = "x", vals = list("a", "b"), is_categorical = TRUE)
  cond <- build_nested_split_condition(split)

  data <- data.frame(x = c("a", "b", "c", "d"))
  result <- dplyr::mutate(data, match = !!cond)$match
  expect_equal(result, c(TRUE, TRUE, FALSE, FALSE))
})

test_that(".build_nested_case_when_tree is exported and works", {
  skip_if_not_installed("rpart")

  model <- rpart::rpart(Sepal.Length ~ Petal.Length, data = iris)
  tree_info <- rpart_tree_info_full(model)

  nested_expr <- .build_nested_case_when_tree(tree_info)

  nested_pred <- dplyr::mutate(iris, pred = !!nested_expr)$pred
  original_pred <- predict(model, iris)

  expect_equal(nested_pred, unname(original_pred))
})

# NA handling tests

test_that("na_handling = 'none' works (default)", {
  skip_if_not_installed("rpart")

  model <- rpart::rpart(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)
  tree_info <- rpart_tree_info_full(model)

  nested_expr <- generate_nested_case_when_tree(tree_info, na_handling = "none")
  nested_pred <- dplyr::mutate(iris, pred = !!nested_expr)$pred
  original_pred <- predict(model, iris)

  expect_equal(nested_pred, unname(original_pred))
})

test_that("na_handling = 'surrogate' matches rpart on data without NAs", {
  skip_if_not_installed("rpart")

  model <- rpart::rpart(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)
  tree_info <- rpart_tree_info_full(model)

  nested_expr <- generate_nested_case_when_tree(
    tree_info,
    na_handling = "surrogate"
  )
  nested_pred <- dplyr::mutate(iris, pred = !!nested_expr)$pred
  original_pred <- predict(model, iris)

  expect_equal(nested_pred, unname(original_pred))
})

test_that("na_handling = 'surrogate' matches rpart on data with NAs", {
  skip_if_not_installed("rpart")

  model <- rpart::rpart(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)
  tree_info <- rpart_tree_info_full(model)

  iris_na <- iris
  iris_na$Petal.Length[c(1, 50, 100)] <- NA
  iris_na$Sepal.Width[c(2, 51)] <- NA

  nested_expr <- generate_nested_case_when_tree(
    tree_info,
    na_handling = "surrogate"
  )
  nested_pred <- dplyr::mutate(iris_na, pred = !!nested_expr)$pred
  original_pred <- predict(model, iris_na)

  expect_equal(nested_pred, unname(original_pred))
})

test_that("na_handling = 'stop' matches rpart with usesurrogate=0", {
  skip_if_not_installed("rpart")

  ctrl <- rpart::rpart.control(usesurrogate = 0)
  model <- rpart::rpart(
    Sepal.Length ~ Sepal.Width + Petal.Length,
    data = iris,
    control = ctrl
  )
  tree_info <- rpart_tree_info_full(model)

  iris_na <- iris
  iris_na$Petal.Length[c(1, 50, 100)] <- NA
  iris_na$Sepal.Width[c(2, 51)] <- NA

  nested_expr <- generate_nested_case_when_tree(tree_info, na_handling = "stop")
  nested_pred <- dplyr::mutate(iris_na, pred = !!nested_expr)$pred
  original_pred <- predict(model, iris_na)

  expect_equal(nested_pred, unname(original_pred))
})

test_that("na_handling validates input", {
  skip_if_not_installed("rpart")

  model <- rpart::rpart(Sepal.Length ~ Petal.Length, data = iris)
  tree_info <- rpart_tree_info_full(model)

  expect_error(
    generate_nested_case_when_tree(tree_info, na_handling = "invalid"),
    "must be one of"
  )
})

# Deep tree tests

test_that("generate_nested_case_when_tree works for deep trees (depth 6+)", {
  skip_if_not_installed("rpart")

  ctrl <- rpart::rpart.control(cp = 0.001, minsplit = 2, maxdepth = 10)
  model <- rpart::rpart(
    Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width,
    data = iris,
    control = ctrl
  )

  tree_info <- rpart_tree_info_full(model)
  n_nodes <- length(tree_info$nodeID)
  expect_gt(n_nodes, 10)

  nested_expr <- generate_nested_case_when_tree(tree_info)
  nested_pred <- dplyr::mutate(iris, pred = !!nested_expr)$pred
  original_pred <- predict(model, iris)

  expect_equal(nested_pred, unname(original_pred))
})

# Nested vs flat comparison tests

test_that("nested and flat produce identical predictions for rpart regression", {
  skip_if_not_installed("rpart")

  model <- rpart::rpart(mpg ~ ., data = mtcars)

  flat_expr <- tidypredict_fit(model, nested = FALSE)
  nested_expr <- tidypredict_fit(model, nested = TRUE)

  flat_pred <- dplyr::mutate(mtcars, pred = !!flat_expr)$pred
  nested_pred <- dplyr::mutate(mtcars, pred = !!nested_expr)$pred

  expect_equal(nested_pred, flat_pred)
})

test_that("nested and flat produce identical predictions for rpart classification", {
  skip_if_not_installed("rpart")

  model <- rpart::rpart(Species ~ ., data = iris, method = "class")

  flat_expr <- tidypredict_fit(model, nested = FALSE)
  nested_expr <- tidypredict_fit(model, nested = TRUE)

  flat_pred <- dplyr::mutate(iris, pred = !!flat_expr)$pred
  nested_pred <- dplyr::mutate(iris, pred = !!nested_expr)$pred

  expect_equal(nested_pred, flat_pred)
})

test_that("nested expression is more compact than flat for deep trees", {
  skip_if_not_installed("rpart")

  ctrl <- rpart::rpart.control(cp = 0.01, minsplit = 2)
  model <- rpart::rpart(Sepal.Length ~ ., data = iris, control = ctrl)

  flat_expr <- tidypredict_fit(model, nested = FALSE)
  nested_expr <- tidypredict_fit(model, nested = TRUE)

  flat_chars <- nchar(rlang::expr_deparse(flat_expr, width = Inf))
  nested_chars <- nchar(rlang::expr_deparse(nested_expr, width = Inf))

  expect_lt(nested_chars, flat_chars)
})
