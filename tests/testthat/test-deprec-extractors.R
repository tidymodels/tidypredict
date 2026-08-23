# Model constructors mirror the ones used in the corresponding test-model-*.R
# files so the deprecated wrappers are exercised on the same fixtures.

deprec_xgb_model <- function() {
  xgb_data <- xgboost::xgb.DMatrix(
    as.matrix(mtcars[, -9]),
    label = mtcars$am
  )

  xgboost::xgb.train(
    params = list(
      max_depth = 2L,
      objective = "reg:squarederror",
      base_score = 0.5
    ),
    data = xgb_data,
    nrounds = 4L,
    verbose = 0
  )
}

deprec_lgb_model <- function() {
  set.seed(123)
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  dtrain <- lightgbm::lgb.Dataset(
    X,
    label = mtcars$hp,
    colnames = c("mpg", "cyl", "disp")
  )
  lightgbm::lgb.train(
    params = list(
      num_leaves = 4L,
      learning_rate = 1.0,
      objective = "regression",
      min_data_in_leaf = 1L
    ),
    data = dtrain,
    nrounds = 10L,
    verbose = -1L
  )
}

deprec_catboost_model <- function() {
  set.seed(123)
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])

  pool <- catboost_catboost.load_pool(
    X,
    label = mtcars$hp,
    feature_names = as.list(c("mpg", "cyl", "disp"))
  )

  catboost_catboost.train(
    pool,
    params = list(
      iterations = 10L,
      depth = 3L,
      learning_rate = 0.5,
      loss_function = "RMSE",
      logging_level = "Silent",
      allow_writing_files = FALSE
    )
  )
}

deprec_earth_multiclass_model <- function() {
  # `earth()` needs its own contrast function on the search path to expand a
  # factor outcome, matching how test-model-earth.R fits these models.
  library(earth)
  suppressWarnings(
    earth::earth(
      Species ~ .,
      data = iris,
      glm = list(family = binomial)
    )
  )
}

skip_if_no_earth_multiclass <- function() {
  skip_if_not_installed("earth")
  skip_if_not(
    exists("contr.earth.response", where = asNamespace("earth")),
    "earth multiclass not available"
  )
}

deprec_multnet_model <- function() {
  glmnet::glmnet(
    as.matrix(iris[, 1:4]),
    iris$Species,
    family = "multinomial",
    lambda = 0.5
  )
}

# .extract_xgb_trees() -------------------------------------------------------

test_that(".extract_xgb_trees() is deprecated", {
  skip_if_not_installed("xgboost")
  model <- deprec_xgb_model()

  expect_snapshot(x <- .extract_xgb_trees(model))
})

test_that(".extract_xgb_trees() still returns the old shape", {
  skip_if_not_installed("xgboost")
  withr::local_options(lifecycle_verbosity = "quiet")
  model <- deprec_xgb_model()

  trees <- .extract_xgb_trees(model)

  expect_type(trees, "list")
  expect_length(trees, 4)
  expect_type(trees[[1]], "language")
})

test_that(".extract_xgb_trees() errors on the wrong class", {
  expect_snapshot(error = TRUE, .extract_xgb_trees(lm(mpg ~ wt, mtcars)))
})

# .extract_lgb_trees() -------------------------------------------------------

test_that(".extract_lgb_trees() is deprecated", {
  skip_if_not_installed("lightgbm")
  model <- deprec_lgb_model()

  expect_snapshot(x <- .extract_lgb_trees(model))
})

test_that(".extract_lgb_trees() still returns the old shape", {
  skip_if_not_installed("lightgbm")
  withr::local_options(lifecycle_verbosity = "quiet")
  model <- deprec_lgb_model()

  trees <- .extract_lgb_trees(model)

  expect_type(trees, "list")
  expect_identical(trees, tidypredict_trees(model))
})

test_that(".extract_lgb_trees() errors on the wrong class", {
  expect_snapshot(error = TRUE, .extract_lgb_trees(lm(mpg ~ wt, mtcars)))
})

# .extract_catboost_trees() --------------------------------------------------

test_that(".extract_catboost_trees() is deprecated", {
  skip_if_not_installed("catboost")
  model <- deprec_catboost_model()

  expect_snapshot(x <- .extract_catboost_trees(model))
})

test_that(".extract_catboost_trees() still returns the old shape", {
  skip_if_not_installed("catboost")
  withr::local_options(lifecycle_verbosity = "quiet")
  model <- deprec_catboost_model()

  trees <- .extract_catboost_trees(model)

  expect_type(trees, "list")
  expect_length(trees, 10)
  expect_type(trees[[1]], "language")
})

test_that(".extract_catboost_trees() errors on the wrong class", {
  expect_snapshot(error = TRUE, .extract_catboost_trees(lm(mpg ~ wt, mtcars)))
})

# .extract_rf_trees() --------------------------------------------------------

test_that(".extract_rf_trees() is deprecated", {
  skip_if_not_installed("randomForest")
  set.seed(123)
  model <- randomForest::randomForest(mpg ~ cyl + disp, mtcars, ntree = 3)

  expect_snapshot(x <- .extract_rf_trees(model))
})

test_that(".extract_rf_trees() still returns the old shape", {
  skip_if_not_installed("randomForest")
  withr::local_options(lifecycle_verbosity = "quiet")
  set.seed(123)
  model <- randomForest::randomForest(mpg ~ cyl + disp, mtcars, ntree = 3)

  trees <- .extract_rf_trees(model)

  expect_type(trees, "list")
  expect_length(trees, 3)
  expect_type(trees[[1]], "language")
})

test_that(".extract_rf_trees() errors on the wrong class", {
  expect_snapshot(error = TRUE, .extract_rf_trees(lm(mpg ~ wt, mtcars)))
})

# .extract_ranger_trees() ----------------------------------------------------

test_that(".extract_ranger_trees() is deprecated", {
  skip_if_not_installed("ranger")
  model <- ranger::ranger(
    mpg ~ cyl + disp,
    data = mtcars,
    num.trees = 3,
    max.depth = 2,
    seed = 100,
    num.threads = 2
  )

  expect_snapshot(x <- .extract_ranger_trees(model))
})

test_that(".extract_ranger_trees() still returns the old shape", {
  skip_if_not_installed("ranger")
  withr::local_options(lifecycle_verbosity = "quiet")
  model <- ranger::ranger(
    mpg ~ cyl + disp,
    data = mtcars,
    num.trees = 3,
    max.depth = 2,
    seed = 100,
    num.threads = 2
  )

  trees <- .extract_ranger_trees(model)

  expect_type(trees, "list")
  expect_length(trees, 3)
  expect_type(trees[[1]], "language")
})

test_that(".extract_ranger_trees() errors on the wrong class", {
  expect_snapshot(error = TRUE, .extract_ranger_trees(lm(mpg ~ wt, mtcars)))
})

# .extract_rf_classprob() ----------------------------------------------------

test_that(".extract_rf_classprob() is deprecated", {
  skip_if_not_installed("randomForest")
  set.seed(123)
  model <- randomForest::randomForest(
    Species ~ Sepal.Length + Sepal.Width,
    data = iris,
    ntree = 3
  )

  expect_snapshot(x <- .extract_rf_classprob(model))
})

test_that(".extract_rf_classprob() still returns the old shape", {
  skip_if_not_installed("randomForest")
  withr::local_options(lifecycle_verbosity = "quiet")
  set.seed(123)
  model <- randomForest::randomForest(
    Species ~ Sepal.Length + Sepal.Width,
    data = iris,
    ntree = 3
  )

  result <- .extract_rf_classprob(model)

  expect_named(result, levels(iris$Species))
  expect_length(result[[1]], 3)
})

test_that(".extract_rf_classprob() errors on the wrong class", {
  expect_snapshot(error = TRUE, .extract_rf_classprob(lm(mpg ~ wt, mtcars)))
})

# .extract_ranger_classprob() ------------------------------------------------

test_that(".extract_ranger_classprob() is deprecated", {
  skip_if_not_installed("ranger")
  model <- ranger::ranger(
    Species ~ Sepal.Length + Sepal.Width,
    data = iris,
    num.trees = 3,
    max.depth = 2,
    seed = 123,
    num.threads = 2,
    probability = TRUE
  )

  expect_snapshot(x <- .extract_ranger_classprob(model))
})

test_that(".extract_ranger_classprob() still returns the old shape", {
  skip_if_not_installed("ranger")
  withr::local_options(lifecycle_verbosity = "quiet")
  model <- ranger::ranger(
    Species ~ Sepal.Length + Sepal.Width,
    data = iris,
    num.trees = 3,
    max.depth = 2,
    seed = 123,
    num.threads = 2,
    probability = TRUE
  )

  result <- .extract_ranger_classprob(model)

  expect_named(result, levels(iris$Species))
  expect_length(result[[1]], 3)
})

test_that(".extract_ranger_classprob() errors on the wrong class", {
  expect_snapshot(error = TRUE, .extract_ranger_classprob(lm(mpg ~ wt, mtcars)))
})

# .extract_rpart_classprob() -------------------------------------------------

test_that(".extract_rpart_classprob() is deprecated", {
  skip_if_not_installed("rpart")
  model <- rpart::rpart(Species ~ Sepal.Length + Sepal.Width, data = iris)

  expect_snapshot(x <- .extract_rpart_classprob(model))
})

test_that(".extract_rpart_classprob() still returns the old shape", {
  skip_if_not_installed("rpart")
  withr::local_options(lifecycle_verbosity = "quiet")
  model <- rpart::rpart(Species ~ Sepal.Length + Sepal.Width, data = iris)

  exprs <- .extract_rpart_classprob(model)

  expect_named(exprs, levels(iris$Species))
  expect_type(exprs[[1]], "language")
})

test_that(".extract_rpart_classprob() errors on the wrong class", {
  expect_snapshot(error = TRUE, .extract_rpart_classprob(lm(mpg ~ wt, mtcars)))
})

# .extract_partykit_classprob() ----------------------------------------------

test_that(".extract_partykit_classprob() is deprecated", {
  skip_if_not_installed("partykit")
  model <- partykit::ctree(Species ~ Sepal.Length + Sepal.Width, data = iris)

  expect_snapshot(x <- .extract_partykit_classprob(model))
})

test_that(".extract_partykit_classprob() still returns an unnamed list", {
  skip_if_not_installed("partykit")
  withr::local_options(lifecycle_verbosity = "quiet")
  model <- partykit::ctree(Species ~ Sepal.Length + Sepal.Width, data = iris)

  exprs <- .extract_partykit_classprob(model)

  expect_null(names(exprs))
  expect_length(exprs, 3)
  expect_type(exprs[[1]], "language")
  expect_identical(exprs, unname(tidypredict_class_exprs(model)))
})

test_that(".extract_partykit_classprob() errors on the wrong class", {
  expect_snapshot(
    error = TRUE,
    .extract_partykit_classprob(lm(mpg ~ wt, mtcars))
  )
})

# .extract_earth_multiclass() ------------------------------------------------

test_that(".extract_earth_multiclass() is deprecated", {
  skip_if_no_earth_multiclass()
  model <- deprec_earth_multiclass_model()

  expect_snapshot(x <- .extract_earth_multiclass(model))
})

test_that(".extract_earth_multiclass() still returns character strings", {
  skip_if_no_earth_multiclass()
  withr::local_options(lifecycle_verbosity = "quiet")
  model <- deprec_earth_multiclass_model()

  result <- .extract_earth_multiclass(model)

  expect_named(result, levels(iris$Species))
  expect_type(result[[1]], "character")
  expect_length(result[[1]], 1)
})

test_that(".extract_earth_multiclass() errors on the wrong class", {
  expect_snapshot(error = TRUE, .extract_earth_multiclass(lm(mpg ~ wt, mtcars)))
})

# .extract_glmnet_multiclass() -----------------------------------------------

test_that(".extract_glmnet_multiclass() is deprecated", {
  skip_if_not_installed("glmnet")
  model <- deprec_multnet_model()

  expect_snapshot(x <- .extract_glmnet_multiclass(model))
})

test_that(".extract_glmnet_multiclass() still returns character strings", {
  skip_if_not_installed("glmnet")
  withr::local_options(lifecycle_verbosity = "quiet")
  model <- deprec_multnet_model()

  result <- .extract_glmnet_multiclass(model)

  expect_named(result, levels(iris$Species))
  expect_type(result[[1]], "character")
  expect_length(result[[1]], 1)
})

test_that(".extract_glmnet_multiclass() accepts a penalty", {
  skip_if_not_installed("glmnet")
  withr::local_options(lifecycle_verbosity = "quiet")
  model <- glmnet::glmnet(
    as.matrix(iris[, 1:4]),
    iris$Species,
    family = "multinomial"
  )

  result <- .extract_glmnet_multiclass(model, penalty = 0.01)

  expect_named(result, levels(iris$Species))
  expect_type(result[[1]], "character")
})

test_that(".extract_glmnet_multiclass() errors on the wrong class", {
  expect_snapshot(
    error = TRUE,
    .extract_glmnet_multiclass(lm(mpg ~ wt, mtcars))
  )
})
