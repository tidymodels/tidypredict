test_that("works with parsnip model specification", {
  etitanic_fac <- dplyr::mutate(earth::etitanic, survived = as.factor(survived))

  # Classification
  model <- parsnip::fit(
    parsnip::set_engine(parsnip::mars(mode = "classification"), "earth"),
    survived ~ age + sibsp,
    data = etitanic_fac
  )

  expect_snapshot(
    tidypredict_test(
      model,
      df = etitanic_fac
    )
  )

  # Regression
  model <- parsnip::fit(
    parsnip::set_engine(parsnip::mars(mode = "regression"), "earth"),
    survived ~ age + sibsp,
    data = etitanic
  )

  expect_snapshot(
    tidypredict_test(
      model,
      df = etitanic_fac
    )
  )
})

test_that("works with decision_tree() and the C5.0 engine", {
  skip_if_not_installed("C50")
  df <- mtcars
  df$vs <- as.factor(df$vs)

  model <- parsnip::fit(
    parsnip::set_engine(
      parsnip::decision_tree(mode = "classification"),
      "C5.0"
    ),
    vs ~ wt + cyl + mpg,
    data = df
  )

  expect_type(tidypredict_fit(model), "language")
  expect_s3_class(tidypredict_sql(model, dbplyr::simulate_dbi()), "sql")
  expect_snapshot(tidypredict_test(model, df = df))
})

test_that("works with C5_rules() and the C5.0 engine", {
  skip_if_not_installed("C50")
  skip_if_not_installed("rules")
  loadNamespace("rules")

  model <- parsnip::fit(
    parsnip::set_engine(
      parsnip::C5_rules(),
      "C5.0"
    ),
    Species ~ .,
    data = iris
  )

  expect_type(tidypredict_fit(model), "language")
  expect_s3_class(tidypredict_sql(model, dbplyr::simulate_dbi()), "sql")
  expect_snapshot(tidypredict_test(model, df = iris))
})

test_that("works with boost_tree() and the C5.0 engine", {
  skip_if_not_installed("C50")

  model <- parsnip::fit(
    parsnip::set_engine(
      parsnip::boost_tree(mode = "classification", trees = 10),
      "C5.0"
    ),
    Species ~ .,
    data = iris
  )

  expect_type(tidypredict_fit(model), "language")
  expect_s3_class(tidypredict_sql(model, dbplyr::simulate_dbi()), "sql")
  expect_snapshot(tidypredict_test(model, df = iris))
})

test_that("works with boost_tree() and the h2o_gbm engine", {
  skip_if_no_h2o()

  reg <- parsnip::fit(
    parsnip::set_engine(
      parsnip::boost_tree(mode = "regression", trees = 10),
      "h2o_gbm"
    ),
    mpg ~ wt + cyl + hp,
    data = mtcars
  )
  expect_type(tidypredict_fit(reg), "language")
  expect_s3_class(tidypredict_sql(reg, dbplyr::simulate_dbi()), "sql")
  expect_false(tidypredict_test(reg, df = mtcars, threshold = 1e-6)$alert)

  df <- mtcars
  df$vs <- factor(df$vs)
  cls <- parsnip::fit(
    parsnip::set_engine(
      parsnip::boost_tree(mode = "classification", trees = 10),
      "h2o_gbm"
    ),
    vs ~ wt + cyl + hp,
    data = df
  )
  expect_type(tidypredict_fit(cls), "language")
  expect_false(tidypredict_test(cls, df = df, threshold = 1e-6)$alert)
})

test_that("works with rule_fit() and the h2o engine", {
  skip_if_no_h2o()

  reg <- parsnip::fit(
    parsnip::set_engine(parsnip::rule_fit(mode = "regression"), "h2o"),
    mpg ~ wt + hp + disp,
    data = mtcars
  )
  expect_type(tidypredict_fit(reg), "language")
  expect_s3_class(tidypredict_sql(reg, dbplyr::simulate_dbi()), "sql")
  expect_false(tidypredict_test(reg, df = mtcars, threshold = 1e-6)$alert)

  df <- mtcars
  df$vs <- factor(df$vs)
  cls <- parsnip::fit(
    parsnip::set_engine(parsnip::rule_fit(mode = "classification"), "h2o"),
    vs ~ wt + hp + disp + mpg,
    data = df
  )
  expect_type(tidypredict_fit(cls), "language")
  expect_false(tidypredict_test(cls, df = df, threshold = 1e-6)$alert)
})

test_that("works with linear_reg() and the glm engine", {
  model <- parsnip::fit(
    parsnip::set_engine(parsnip::linear_reg(), "glm"),
    mpg ~ wt + cyl,
    data = mtcars
  )

  expect_type(tidypredict_fit(model), "language")

  expect_snapshot(
    tidypredict_test(model, df = mtcars)
  )
})

test_that("works with logistic_reg() and the LiblineaR engine", {
  skip_if_not_installed("LiblineaR")

  df <- mtcars
  df$am <- factor(df$am)

  ridge <- parsnip::fit(
    parsnip::set_engine(
      parsnip::logistic_reg(penalty = 0.1, mixture = 0),
      "LiblineaR"
    ),
    am ~ mpg + cyl + hp,
    data = df
  )
  lasso <- parsnip::fit(
    parsnip::set_engine(
      parsnip::logistic_reg(penalty = 0.1, mixture = 1),
      "LiblineaR"
    ),
    am ~ mpg + cyl + hp,
    data = df
  )

  for (model in list(ridge, lasso)) {
    expect_type(tidypredict_fit(model), "language")
    expect_false(tidypredict_test(model, df = df)$alert)
    expect_s3_class(
      tidypredict_sql(model, dbplyr::simulate_dbi()),
      "sql"
    )
  }
})

test_that("works with svm_linear() and the LiblineaR engine", {
  skip_if_not_installed("LiblineaR")

  reg <- parsnip::fit(
    parsnip::set_engine(
      parsnip::svm_linear(mode = "regression"),
      "LiblineaR"
    ),
    mpg ~ wt + hp + disp,
    data = mtcars
  )

  df <- mtcars
  df$am <- factor(ifelse(df$am == 1, "yes", "no"))
  cls <- parsnip::fit(
    parsnip::set_engine(
      parsnip::svm_linear(mode = "classification"),
      "LiblineaR"
    ),
    am ~ wt + hp + disp,
    data = df
  )

  for (model in list(reg, cls)) {
    expect_type(tidypredict_fit(model), "language")
    expect_s3_class(
      tidypredict_sql(model, dbplyr::simulate_dbi()),
      "sql"
    )
  }
  expect_false(tidypredict_test(reg, df = mtcars)$alert)
  expect_false(tidypredict_test(cls, df = df)$alert)
})

test_that("works with svm_linear() and the kernlab engine", {
  skip_if_not_installed("kernlab")

  reg <- parsnip::fit(
    parsnip::set_engine(
      parsnip::svm_linear(mode = "regression"),
      "kernlab"
    ),
    mpg ~ wt + hp + disp,
    data = mtcars
  )

  df <- mtcars
  df$am <- factor(ifelse(df$am == 1, "yes", "no"))
  cls <- parsnip::fit(
    parsnip::set_engine(
      parsnip::svm_linear(mode = "classification"),
      "kernlab"
    ),
    am ~ wt + hp + disp,
    data = df
  )

  for (model in list(reg, cls)) {
    expect_type(tidypredict_fit(model), "language")
    expect_s3_class(
      tidypredict_sql(model, dbplyr::simulate_dbi()),
      "sql"
    )
  }
  expect_false(tidypredict_test(reg, df = mtcars)$alert)
  expect_false(tidypredict_test(cls, df = df)$alert)
})

test_that("works with decision_tree() and the rpart engine", {
  skip_if_not_installed("rpart")

  # Regression
  reg <- parsnip::fit(
    parsnip::set_mode(
      parsnip::set_engine(parsnip::decision_tree(), "rpart"),
      "regression"
    ),
    mpg ~ wt + cyl,
    data = mtcars
  )

  expect_type(tidypredict_fit(reg), "language")
  expect_s3_class(tidypredict_sql(reg, dbplyr::simulate_dbi()), "sql")
  expect_snapshot(tidypredict_test(reg, df = mtcars))

  # Classification
  df <- mtcars
  df$am <- factor(df$am)
  cls <- parsnip::fit(
    parsnip::set_mode(
      parsnip::set_engine(parsnip::decision_tree(), "rpart"),
      "classification"
    ),
    am ~ mpg + cyl + hp,
    data = df
  )

  expect_type(tidypredict_fit(cls), "language")
  expect_s3_class(tidypredict_sql(cls, dbplyr::simulate_dbi()), "sql")
})

test_that("works with rand_forest() and the partykit engine", {
  skip_if_not_installed("bonsai")
  skip_if_not_installed("partykit")

  set.seed(1)
  reg <- parsnip::fit(
    parsnip::set_engine(
      parsnip::rand_forest(mode = "regression", trees = 20),
      "partykit"
    ),
    mpg ~ wt + cyl,
    data = mtcars
  )

  expect_type(tidypredict_fit(reg), "language")
  expect_s3_class(tidypredict_sql(reg, dbplyr::simulate_dbi()), "sql")
  expect_snapshot(tidypredict_test(reg, df = mtcars))

  # Classification is not supported
  cls <- parsnip::fit(
    parsnip::set_engine(
      parsnip::rand_forest(mode = "classification", trees = 5),
      "partykit"
    ),
    Species ~ .,
    data = iris
  )
  expect_snapshot(error = TRUE, tidypredict_fit(cls))
})

test_that("works with rand_forest() and the aorsf engine", {
  skip_if_not_installed("bonsai")
  skip_if_not_installed("aorsf")

  set.seed(1)
  reg <- parsnip::fit(
    parsnip::set_engine(
      parsnip::rand_forest(mode = "regression", trees = 20),
      "aorsf"
    ),
    mpg ~ wt + cyl + disp,
    data = mtcars
  )

  expect_type(tidypredict_fit(reg), "language")
  expect_s3_class(tidypredict_sql(reg, dbplyr::simulate_dbi()), "sql")

  # aorsf uses observed split values as cutpoints, so agreement is checked on
  # jittered data to avoid exact training-row boundary ties.
  set.seed(99)
  nd <- mtcars
  nd[] <- lapply(mtcars, function(x) x + rnorm(length(x), 0, 0.01))
  expect_false(tidypredict_test(reg, df = nd)$alert)

  # Classification is not supported
  cls <- parsnip::fit(
    parsnip::set_engine(
      parsnip::rand_forest(mode = "classification", trees = 5),
      "aorsf"
    ),
    Species ~ .,
    data = iris
  )
  expect_snapshot(error = TRUE, tidypredict_fit(cls))
})

test_that("works with linear_reg() and the quantreg engine", {
  skip_if_not_installed("quantreg")

  model <- parsnip::fit(
    parsnip::set_mode(
      parsnip::set_engine(parsnip::linear_reg(), "quantreg"),
      "quantile regression",
      quantile_levels = 0.5
    ),
    mpg ~ wt + cyl,
    data = mtcars
  )

  expect_type(tidypredict_fit(model), "language")

  expect_snapshot(
    tidypredict_test(model, df = mtcars)
  )
})

test_that("works with rule_fit() and the xrf engine", {
  skip_if_not_installed("rules")
  skip_if_not_installed("xrf")
  # {rules} must be attached for parsnip's xrf prediction to resolve.
  withr::local_package("rules")

  df <- mtcars
  df$cyl <- factor(df$cyl)

  set.seed(1)
  reg <- parsnip::fit(
    parsnip::set_engine(
      parsnip::rule_fit(mode = "regression", trees = 5, penalty = 0.1),
      "xrf"
    ),
    mpg ~ wt + hp + cyl,
    data = df
  )

  cls_df <- mtcars
  cls_df$am <- factor(ifelse(cls_df$am == 1, "yes", "no"))
  set.seed(1)
  cls <- parsnip::fit(
    parsnip::set_engine(
      parsnip::rule_fit(mode = "classification", trees = 5, penalty = 0.01),
      "xrf"
    ),
    am ~ wt + hp + disp,
    data = cls_df
  )

  for (model in list(reg, cls)) {
    expect_type(tidypredict_fit(model), "language")
    expect_s3_class(
      tidypredict_sql(model, dbplyr::simulate_dbi()),
      "sql"
    )
  }

  # The tuned `penalty` is used, not the cross-validated minimum.
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(reg), df),
    as.numeric(predict(reg, df)$.pred)
  )
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(cls), cls_df),
    predict(cls, cls_df, type = "prob")$.pred_yes
  )
})

test_that("mlp is handled with parsnip", {
  skip_if_not_installed("nnet")
  skip_if_not_installed("parsnip")

  set.seed(100)
  reg <- parsnip::fit(
    parsnip::mlp(mode = "regression", hidden_units = 3, epochs = 100),
    mpg ~ wt + hp,
    data = mtcars
  )

  expect_type(tidypredict_fit(reg), "language")
  expect_s3_class(tidypredict_sql(reg, dbplyr::simulate_dbi()), "sql")
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(reg), mtcars),
    predict(reg, mtcars)$.pred
  )

  set.seed(100)
  cls <- parsnip::fit(
    parsnip::mlp(mode = "classification", hidden_units = 3, epochs = 100),
    Species ~ .,
    data = iris
  )

  tf <- tidypredict_fit(cls)
  expect_named(tf, levels(iris$Species))

  # parsnip runs the probabilities of `predict.nnet()` through a second softmax
  probs <- sapply(tf, \(f) rlang::eval_tidy(f, iris))
  expect_equal(
    unname(probs),
    unname(as.matrix(predict(cls, iris, type = "prob")))
  )
})

test_that("bart is handled with parsnip", {
  skip_if_not_installed("dbarts")
  skip_if_not_installed("parsnip")

  set.seed(100)
  df <- data.frame(x1 = rnorm(60), x2 = rnorm(60))
  df$y <- 2 * df$x1 - df$x2 + rnorm(60)

  reg <- parsnip::fit(
    parsnip::set_engine(
      parsnip::bart(mode = "regression", trees = 3),
      "dbarts",
      ndpost = 4,
      nskip = 10,
      nchain = 1,
      nthread = 1,
      verbose = FALSE
    ),
    y ~ .,
    data = df
  )

  expect_type(tidypredict_fit(reg), "language")
  expect_s3_class(tidypredict_sql(reg, dbplyr::simulate_dbi()), "sql")

  # `predict()` draws from the posterior predictive distribution, which adds
  # residual noise, so the comparison is against the expected value
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(reg), df),
    colMeans(predict(reg$fit, df, type = "ev"))
  )

  cls_df <- df
  cls_df$y <- factor(ifelse(df$y > 0, "yes", "no"))
  cls <- parsnip::fit(
    parsnip::set_engine(
      parsnip::bart(mode = "classification", trees = 3),
      "dbarts",
      ndpost = 4,
      nskip = 10,
      nchain = 1,
      nthread = 1,
      verbose = FALSE
    ),
    y ~ .,
    data = cls_df
  )

  expect_snapshot(error = TRUE, tidypredict_fit(cls))
})

test_that("bag_tree is handled with parsnip", {
  skip_if_not_installed("baguette")
  skip_if_not_installed("parsnip")

  set.seed(100)
  reg <- parsnip::fit(
    parsnip::set_engine(
      parsnip::bag_tree(mode = "regression"),
      "rpart",
      times = 3
    ),
    mpg ~ wt + cyl + disp,
    data = mtcars
  )

  expect_type(tidypredict_fit(reg), "language")
  expect_s3_class(tidypredict_sql(reg, dbplyr::simulate_dbi()), "sql")
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(reg), mtcars),
    predict(reg, mtcars)$.pred
  )

  set.seed(100)
  cls <- parsnip::fit(
    parsnip::set_engine(
      parsnip::bag_tree(mode = "classification"),
      "rpart",
      times = 3
    ),
    Species ~ .,
    data = iris
  )

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(cls), iris),
    as.character(predict(cls, iris)$.pred_class)
  )
})

test_that("bag_tree is handled with parsnip and the C5.0 engine", {
  skip_if_not_installed("baguette")
  skip_if_not_installed("parsnip")
  skip_if_not_installed("C50")

  set.seed(100)
  cls <- parsnip::fit(
    parsnip::set_engine(
      parsnip::bag_tree(mode = "classification"),
      "C5.0",
      times = 3
    ),
    Species ~ .,
    data = iris
  )

  expect_type(tidypredict_fit(cls), "language")
  expect_s3_class(tidypredict_sql(cls, dbplyr::simulate_dbi()), "sql")
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(cls), iris),
    as.character(predict(cls, iris)$.pred_class)
  )
})
