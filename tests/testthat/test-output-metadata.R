test_that("regression models report numeric output", {
  expect_identical(tidypredict_output_type(lm(mpg ~ wt, mtcars)), "numeric")
  expect_identical(
    tidypredict_output_type(glm(mpg ~ wt, data = mtcars)),
    "numeric"
  )
})

test_that("only binomial glms report prob, not every glm", {
  binom <- glm(vs ~ wt, data = mtcars, family = binomial)
  pois <- glm(carb ~ wt, data = mtcars, family = poisson)

  expect_identical(tidypredict_output_type(binom), "prob")
  expect_identical(tidypredict_output_type(pois), "numeric")

  # The claim is about values, not labels: a binomial fit lands in [0, 1] and
  # a Poisson one does not, so calling both "prob" would be wrong.
  binom_vals <- rlang::eval_tidy(tidypredict_fit(binom), mtcars)
  pois_vals <- rlang::eval_tidy(tidypredict_fit(pois), mtcars)

  expect_true(all(binom_vals >= 0 & binom_vals <= 1))
  expect_false(all(pois_vals >= 0 & pois_vals <= 1))
})

test_that("a single expression has no per-level sum to normalize", {
  expect_identical(tidypredict_normalized(lm(mpg ~ wt, mtcars)), NA)
})

test_that("LiblineaR distinguishes probability from decision value", {
  skip_if_not_installed("LiblineaR")

  df <- iris[iris$Species != "virginica", ]
  df$Species <- droplevels(df$Species)
  x <- as.matrix(df[, 1:4])

  lr <- LiblineaR::LiblineaR(data = x, target = df$Species, type = 0)
  svm <- LiblineaR::LiblineaR(data = x, target = df$Species, type = 1)

  expect_identical(tidypredict_output_type(lr), "prob")
  expect_identical(tidypredict_output_type(svm), "decision")

  # Same shape, one expression each, but different meaning. The logistic fit
  # lies in [0, 1]; the SVM decision value straddles 0, so thresholding it at
  # 0.5 would misclassify every row between 0 and 0.5.
  lr_vals <- rlang::eval_tidy(tidypredict_fit(lr), df)
  svm_vals <- rlang::eval_tidy(tidypredict_fit(svm), df)

  expect_true(all(lr_vals >= 0 & lr_vals <= 1))
  expect_false(all(svm_vals >= 0 & svm_vals <= 1))
  expect_true(any(svm_vals < 0))
})

test_that("LiblineaR reports outcome levels for classifiers only", {
  skip_if_not_installed("LiblineaR")

  df <- iris[iris$Species != "virginica", ]
  df$Species <- droplevels(df$Species)
  x <- as.matrix(df[, 1:4])

  svm <- LiblineaR::LiblineaR(data = x, target = df$Species, type = 1)
  expect_identical(
    tidypredict_outcome_levels(svm),
    c("setosa", "versicolor")
  )

  # svr_eps is passed explicitly only to silence LiblineaR's default notice.
  svr <- LiblineaR::LiblineaR(
    data = x,
    target = df$Sepal.Length,
    type = 11,
    svr_eps = 0.1
  )
  expect_identical(tidypredict_output_type(svr), "numeric")
  expect_null(tidypredict_outcome_levels(svr))
})

test_that("models with no metadata method say so", {
  expect_snapshot(
    error = TRUE,
    tidypredict_output_type(structure(list(), class = "made_up_model"))
  )

  expect_error(
    tidypredict_output_type(structure(list(), class = "made_up_model")),
    class = "tidypredict_no_metadata"
  )
})

test_that("a parsed model with no method does not recurse", {
  pm <- as_parsed_model(list(general = list(type = "made_up")))

  expect_error(
    tidypredict_output_type(pm),
    class = "tidypredict_no_metadata"
  )
})

# Value-level semantics ---------------------------

eval_metadata_fit <- function(fit, df) {
  if (!is.list(fit)) {
    return(rlang::eval_tidy(fit, df))
  }
  do.call(cbind, lapply(fit, function(e) rlang::eval_tidy(e, df)))
}

test_that("a prob list really does sum to one across levels", {
  skip_if_not_installed("nnet")

  model <- nnet::multinom(Species ~ ., data = iris, trace = FALSE)

  expect_identical(tidypredict_output_type(model), "prob")
  expect_identical(tidypredict_normalized(model), TRUE)
  expect_identical(tidypredict_outcome_levels(model), levels(iris$Species))

  probs <- eval_metadata_fit(tidypredict_fit(model), iris)
  expect_equal(rowSums(probs), rep(1, nrow(iris)), tolerance = 1e-8)
})

test_that("a numeric list of the same shape does not sum to one", {
  skip_if_not_installed("quantreg")

  model <- quantreg::rq(mpg ~ wt, tau = c(0.25, 0.5, 0.75), data = mtcars)

  expect_identical(tidypredict_output_type(model), "numeric")
  expect_null(tidypredict_outcome_levels(model))
  expect_identical(tidypredict_normalized(model), NA)

  # `parse_model_lm()` calls `summary()` on each single-quantile fit, which
  # warns about a nonunique solution and is unrelated to the metadata.
  fit <- suppressWarnings(tidypredict_fit(model))
  quantiles <- eval_metadata_fit(fit, mtcars)
  expect_length(colnames(quantiles), 3)
  # The point of the metadata: same shape as a multiclass probability list,
  # so treating it as one would be wrong.
  expect_false(any(abs(rowSums(quantiles) - 1) < 1))
})

test_that("a class model returns labels drawn from its levels", {
  skip_if_not_installed("rpart")

  model <- rpart::rpart(Species ~ ., data = iris, method = "class")
  levs <- tidypredict_outcome_levels(model)

  expect_identical(tidypredict_output_type(model), "class")
  expect_identical(levs, levels(iris$Species))
  expect_identical(tidypredict_normalized(model), NA)

  predicted <- eval_metadata_fit(tidypredict_fit(model), iris)
  expect_type(predicted, "character")
  expect_in(unique(predicted), levs)
})

test_that("a single prob expression stays inside [0, 1]", {
  skip_if_not_installed("glmnet")

  df <- iris[iris$Species != "virginica", ]
  df$Species <- droplevels(df$Species)
  model <- glmnet::glmnet(
    as.matrix(df[, 1:4]),
    df$Species,
    family = "binomial",
    lambda = 0.01
  )

  expect_identical(tidypredict_output_type(model), "prob")
  expect_identical(tidypredict_outcome_levels(model), levels(df$Species))
  expect_identical(tidypredict_normalized(model), NA)

  probs <- eval_metadata_fit(tidypredict_fit(model), df)
  expect_all_true(probs >= 0 & probs <= 1)
})

# Linear and additive backends --------------------

test_that("glm only reports levels for a factor binomial outcome", {
  df <- iris[iris$Species != "virginica", ]
  df$Species <- droplevels(df$Species)

  factor_fit <- glm(Species ~ Sepal.Length, data = df, family = binomial)
  numeric_fit <- glm(vs ~ wt, data = mtcars, family = binomial)

  expect_identical(tidypredict_outcome_levels(factor_fit), levels(df$Species))
  # Fit on a 0/1 numeric, so the model kept no levels to report.
  expect_null(tidypredict_outcome_levels(numeric_fit))
  expect_null(tidypredict_outcome_levels(glm(mpg ~ wt, data = mtcars)))
})

test_that("glmnet families report the right type", {
  skip_if_not_installed("glmnet")

  gaussian <- glmnet::glmnet(mtcars[, -1], mtcars$mpg, lambda = 1)
  multinomial <- glmnet::glmnet(
    as.matrix(iris[, 1:4]),
    iris$Species,
    family = "multinomial",
    lambda = 0.01
  )

  expect_identical(tidypredict_output_type(gaussian), "numeric")
  expect_null(tidypredict_outcome_levels(gaussian))

  expect_identical(tidypredict_output_type(multinomial), "prob")
  expect_identical(
    tidypredict_outcome_levels(multinomial),
    levels(iris$Species)
  )
  expect_identical(tidypredict_normalized(multinomial), TRUE)
})

test_that("earth and xrf report numeric", {
  skip_if_not_installed("earth")

  expect_identical(
    tidypredict_output_type(earth::earth(mpg ~ ., data = mtcars)),
    "numeric"
  )
})

test_that("ksvm distinguishes regression from binary classification", {
  skip_if_not_installed("kernlab")

  df <- iris[iris$Species != "virginica", ]
  df$Species <- droplevels(df$Species)

  svr <- kernlab::ksvm(
    mpg ~ .,
    data = mtcars,
    kernel = "vanilladot",
    type = "eps-svr"
  )
  svc <- kernlab::ksvm(
    Species ~ .,
    data = df,
    kernel = "vanilladot",
    prob.model = TRUE
  )

  expect_identical(tidypredict_output_type(svr), "numeric")
  # `lev()` holds the sorted response values for an SVR, which are not levels.
  expect_null(tidypredict_outcome_levels(svr))

  expect_identical(tidypredict_output_type(svc), "prob")
  expect_identical(tidypredict_outcome_levels(svc), levels(df$Species))
  expect_identical(tidypredict_normalized(svc), NA)
})

test_that("nullmodel reports its mode", {
  skip_if_not_installed("parsnip")

  reg <- parsnip::nullmodel(mtcars[, -1], mtcars$mpg)
  cls <- parsnip::nullmodel(iris[, 1:4], iris$Species)

  expect_identical(tidypredict_output_type(reg), "numeric")
  expect_null(tidypredict_outcome_levels(reg))
  expect_identical(tidypredict_normalized(reg), NA)

  expect_identical(tidypredict_output_type(cls), "prob")
  expect_identical(tidypredict_outcome_levels(cls), levels(iris$Species))
  expect_identical(tidypredict_normalized(cls), TRUE)
})

test_that("mixOmics separates regression from discriminant analysis", {
  skip_if_not_installed("mixOmics")

  pls <- mixOmics::pls(as.matrix(iris[, 1:3]), iris[, 4], ncomp = 2)
  plsda <- mixOmics::plsda(as.matrix(iris[, 1:4]), iris$Species, ncomp = 2)

  expect_identical(tidypredict_output_type(pls), "numeric")
  expect_null(tidypredict_outcome_levels(pls))

  expect_identical(tidypredict_output_type(plsda), "prob")
  expect_identical(tidypredict_outcome_levels(plsda), levels(iris$Species))
  expect_identical(tidypredict_normalized(plsda), TRUE)
})

# Discriminant and naive Bayes backends -----------

test_that("discriminant backends report a normalized prob list", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("mda")
  skip_if_not_installed("sda")
  skip_if_not_installed("sparsediscrim")

  models <- list(
    lda = MASS::lda(Species ~ ., data = iris),
    qda = MASS::qda(Species ~ ., data = iris),
    fda = mda::fda(Species ~ ., data = iris),
    sda = sda::sda(as.matrix(iris[, 1:4]), iris$Species, verbose = FALSE),
    lda_diag = sparsediscrim::lda_diag(Species ~ ., data = iris)
  )

  expect_all_equal(
    vapply(models, tidypredict_output_type, character(1)),
    "prob"
  )
  expect_all_true(vapply(models, tidypredict_normalized, logical(1)))
  for (model in models) {
    expect_identical(tidypredict_outcome_levels(model), levels(iris$Species))
  }
})

test_that("naive Bayes backends report a normalized prob list", {
  skip_if_not_installed("naivebayes")
  skip_if_not_installed("klaR")

  nb <- naivebayes::naive_bayes(Species ~ ., data = iris)
  knb <- klaR::NaiveBayes(Species ~ ., data = iris)

  expect_identical(tidypredict_output_type(nb), "prob")
  expect_identical(tidypredict_output_type(knb), "prob")
  expect_identical(tidypredict_outcome_levels(nb), levels(iris$Species))
  expect_identical(tidypredict_outcome_levels(knb), levels(iris$Species))
  expect_identical(tidypredict_normalized(nb), TRUE)
  expect_identical(tidypredict_normalized(knb), TRUE)
})

test_that("nnet reports its mode", {
  skip_if_not_installed("nnet")

  cls <- nnet::nnet(Species ~ ., data = iris, size = 1, trace = FALSE)
  reg <- nnet::nnet(
    mpg ~ wt,
    data = mtcars,
    size = 1,
    trace = FALSE,
    linout = TRUE
  )

  expect_identical(tidypredict_output_type(cls), "prob")
  expect_identical(tidypredict_outcome_levels(cls), levels(iris$Species))
  expect_identical(tidypredict_normalized(cls), TRUE)

  expect_identical(tidypredict_output_type(reg), "numeric")
  expect_null(tidypredict_outcome_levels(reg))
  expect_identical(tidypredict_normalized(reg), NA)
})

# Tree backends ----------------------------------

test_that("regression-only forests report numeric", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("randomForest")
  skip_if_not_installed("Cubist")

  models <- list(
    ranger = ranger::ranger(mpg ~ ., data = mtcars, num.trees = 2),
    randomForest = randomForest::randomForest(mpg ~ ., mtcars, ntree = 2),
    cubist = Cubist::cubist(mtcars[, -1], mtcars$mpg)
  )

  expect_all_equal(
    vapply(models, tidypredict_output_type, character(1)),
    "numeric"
  )
  for (model in models) {
    expect_null(tidypredict_outcome_levels(model))
    expect_identical(tidypredict_normalized(model), NA)
  }
})

test_that("rpart reports its mode from `method`", {
  skip_if_not_installed("rpart")

  reg <- rpart::rpart(mpg ~ wt, data = mtcars)

  expect_identical(tidypredict_output_type(reg), "numeric")
  expect_null(tidypredict_outcome_levels(reg))
  expect_identical(tidypredict_normalized(reg), NA)
})

test_that("partykit ctree reports its mode from the response", {
  skip_if_not_installed("partykit")

  reg <- partykit::ctree(mpg ~ wt, data = mtcars)
  cls <- partykit::ctree(Species ~ ., data = iris)

  expect_identical(tidypredict_output_type(reg), "numeric")
  expect_null(tidypredict_outcome_levels(reg))

  expect_identical(tidypredict_output_type(cls), "class")
  expect_identical(tidypredict_outcome_levels(cls), levels(iris$Species))
  expect_identical(tidypredict_normalized(cls), NA)
})

test_that("C5.0 reports a class label", {
  skip_if_not_installed("C50")

  model <- C50::C5.0(iris[, 1:4], iris$Species, trials = 2)

  expect_identical(tidypredict_output_type(model), "class")
  expect_identical(tidypredict_outcome_levels(model), levels(iris$Species))
  expect_identical(tidypredict_normalized(model), NA)
})

test_that("bagger reports its mode", {
  skip_if_not_installed("baguette")

  reg <- baguette::bagger(mpg ~ wt + cyl, data = mtcars, times = 2)
  cls <- baguette::bagger(Species ~ ., data = iris, times = 2)

  expect_identical(tidypredict_output_type(reg), "numeric")
  expect_null(tidypredict_outcome_levels(reg))

  expect_identical(tidypredict_output_type(cls), "class")
  expect_identical(tidypredict_outcome_levels(cls), levels(iris$Species))
  expect_identical(tidypredict_normalized(cls), NA)
})

test_that("aorsf and mboost report numeric", {
  skip_if_not_installed("aorsf")
  skip_if_not_installed("mboost")

  orsf <- aorsf::orsf(mtcars, mpg ~ ., n_tree = 2)
  bb <- mboost::blackboost(mpg ~ wt, data = mtcars)

  expect_identical(tidypredict_output_type(orsf), "numeric")
  expect_identical(tidypredict_output_type(bb), "numeric")
  expect_null(tidypredict_outcome_levels(orsf))
  expect_null(tidypredict_outcome_levels(bb))
  expect_identical(tidypredict_normalized(orsf), NA)
  expect_identical(tidypredict_normalized(bb), NA)
})

test_that("dbarts reports numeric", {
  skip_if_not_installed("dbarts")

  model <- dbarts::bart2(
    mpg ~ wt,
    data = mtcars,
    n.trees = 5,
    n.samples = 5,
    n.burn = 5,
    verbose = FALSE,
    keepTrees = TRUE
  )

  expect_identical(tidypredict_output_type(model), "numeric")
  expect_null(tidypredict_outcome_levels(model))
  expect_identical(tidypredict_normalized(model), NA)
})

# Boosted backends -------------------------------

test_that("xgboost reads its mode from the objective", {
  skip_if_not_installed("xgboost")

  reg <- xgboost::xgb.train(
    list(objective = "reg:squarederror", max_depth = 2),
    xgboost::xgb.DMatrix(as.matrix(mtcars[, -1]), label = mtcars$mpg),
    nrounds = 2,
    verbose = 0
  )
  binary <- xgboost::xgb.train(
    list(objective = "binary:logistic", max_depth = 2),
    xgboost::xgb.DMatrix(as.matrix(mtcars[, -8]), label = mtcars$vs),
    nrounds = 2,
    verbose = 0
  )

  expect_identical(tidypredict_output_type(reg), "numeric")
  expect_identical(tidypredict_output_type(binary), "prob")
  # xgboost is fit on a numeric label, so no fit records outcome levels.
  expect_null(tidypredict_outcome_levels(binary))
  expect_identical(tidypredict_normalized(binary), NA)
})

test_that("xgboost binary:hinge is a class prediction, not a number", {
  skip_if_not_installed("xgboost")

  hinge <- xgboost::xgb.train(
    list(objective = "binary:hinge", max_depth = 2),
    xgboost::xgb.DMatrix(as.matrix(mtcars[, -8]), label = mtcars$vs),
    nrounds = 3,
    verbose = 0
  )

  expect_identical(tidypredict_output_type(hinge), "class")

  # The label follows from the values: `as.numeric(score >= 0)` can only ever
  # be 0 or 1, so treating it as a numeric prediction would be wrong even
  # though its type is numeric.
  values <- rlang::eval_tidy(tidypredict_fit(hinge), mtcars)
  expect_setequal(unique(values), c(0, 1))
})

test_that("lightgbm reads its mode from the objective", {
  skip_if_not_installed("lightgbm")

  reg <- lightgbm::lgb.train(
    list(objective = "regression", num_leaves = 3, verbose = -1),
    lightgbm::lgb.Dataset(as.matrix(mtcars[, -1]), label = mtcars$mpg),
    nrounds = 2,
    verbose = -1
  )
  binary <- lightgbm::lgb.train(
    list(objective = "binary", num_leaves = 3, verbose = -1),
    lightgbm::lgb.Dataset(as.matrix(mtcars[, -8]), label = mtcars$vs),
    nrounds = 2,
    verbose = -1
  )
  multi <- lightgbm::lgb.train(
    list(objective = "multiclass", num_class = 3, num_leaves = 3, verbose = -1),
    lightgbm::lgb.Dataset(
      as.matrix(iris[, 1:4]),
      label = as.integer(iris$Species) - 1
    ),
    nrounds = 2,
    verbose = -1
  )

  expect_identical(tidypredict_output_type(reg), "numeric")
  expect_identical(tidypredict_normalized(reg), NA)

  expect_identical(tidypredict_output_type(binary), "prob")
  expect_identical(tidypredict_normalized(binary), NA)

  expect_identical(tidypredict_output_type(multi), "prob")
  expect_identical(tidypredict_normalized(multi), TRUE)
  # LightGBM stores integer labels, so the `class_*` names are positional and
  # the caller has to supply the real levels.
  expect_null(tidypredict_outcome_levels(multi))
  expect_named(tidypredict_fit(multi), c("class_0", "class_1", "class_2"))
})

test_that("catboost reads its mode from the objective", {
  skip_if_not_installed("catboost")

  train <- function(X, y, loss_function) {
    pool <- catboost_catboost.load_pool(
      X,
      label = y,
      feature_names = as.list(colnames(X))
    )
    catboost_catboost.train(
      pool,
      params = list(
        iterations = 5L,
        depth = 2L,
        learning_rate = 0.5,
        loss_function = loss_function,
        logging_level = "Silent",
        allow_writing_files = FALSE
      )
    )
  }

  reg <- train(
    data.matrix(mtcars[, c("cyl", "disp")]),
    mtcars$mpg,
    "RMSE"
  )
  multi <- train(
    data.matrix(iris[, 1:4]),
    as.integer(iris$Species) - 1L,
    "MultiClass"
  )

  expect_identical(tidypredict_output_type(reg), "numeric")
  expect_identical(tidypredict_normalized(reg), NA)
  expect_null(tidypredict_outcome_levels(reg))

  expect_identical(tidypredict_output_type(multi), "prob")
  expect_identical(tidypredict_normalized(multi), TRUE)
  # CatBoost stores integer labels, so the `class_*` names are positional.
  expect_null(tidypredict_outcome_levels(multi))
})

# Parsed models ----------------------------------

test_that("an rpart parsed model cannot say which mode it came from", {
  skip_if_not_installed("rpart")

  pm <- parse_model(rpart::rpart(Species ~ ., data = iris, method = "class"))

  expect_snapshot(error = TRUE, tidypredict_output_type(pm))
})
