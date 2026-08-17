test_that("returns the right output", {
  skip_if_not_installed("nnet")

  model <- nnet::multinom(Species ~ ., data = iris, trace = FALSE)

  tf <- tidypredict_fit(model)
  expect_type(tf, "list")
  expect_named(tf, levels(iris$Species))
  expect_true(all(vapply(tf, is.language, logical(1))))

  pm <- parse_model(model)
  expect_s3_class(pm, "list")
  expect_equal(length(pm), 3)
  expect_equal(pm$general$model, "multinom")
  expect_equal(pm$general$version, 2)
  expect_equal(pm$classes, levels(iris$Species))

  lps <- lapply(pm$class_terms, build_linear_predictor)
  expect_snapshot(lapply(lps, round_print))
})

test_that("predictions match native predict", {
  skip_if_not_installed("nnet")

  model <- nnet::multinom(Species ~ ., data = iris, trace = FALSE)

  probs <- sapply(tidypredict_fit(model), \(f) rlang::eval_tidy(f, iris))
  native <- predict(model, iris, type = "probs")

  expect_equal(unname(probs), unname(native))
  expect_equal(unname(rowSums(probs)), rep(1, nrow(iris)))
})

test_that("categorical predictors are handled", {
  skip_if_not_installed("nnet")

  df <- transform(mtcars, cyl = factor(cyl), gear = factor(gear))
  model <- nnet::multinom(cyl ~ mpg + gear + disp, data = df, trace = FALSE)

  probs <- sapply(tidypredict_fit(model), \(f) rlang::eval_tidy(f, df))

  expect_equal(unname(probs), unname(predict(model, df, type = "probs")))
})

test_that("binary outcomes are handled", {
  skip_if_not_installed("nnet")

  df <- transform(mtcars, am = factor(am))
  model <- nnet::multinom(am ~ mpg + wt, data = df, trace = FALSE)

  tf <- tidypredict_fit(model)
  expect_named(tf, c("0", "1"))

  probs <- sapply(tf, \(f) rlang::eval_tidy(f, df))

  expect_equal(unname(probs[, 2]), unname(predict(model, df, type = "probs")))
})

test_that("interactions and weights are handled", {
  skip_if_not_installed("nnet")

  df <- transform(mtcars, cyl = factor(cyl))
  model <- nnet::multinom(
    cyl ~ mpg * wt,
    data = df,
    weights = rep(c(1, 2), 16),
    trace = FALSE
  )

  probs <- sapply(tidypredict_fit(model), \(f) rlang::eval_tidy(f, df))

  expect_equal(unname(probs), unname(predict(model, df, type = "probs")))
})

multinom_df <- function(levels_of = identity, ordered = FALSE) {
  set.seed(1)
  df <- data.frame(x = rnorm(200), z = rnorm(200))
  g <- rep(c("a", "b", "c"), length.out = 200)
  df$f <- factor(levels_of(g), ordered = ordered)
  score <- rnorm(200) + df$x + as.numeric(factor(g))
  df$y <- factor(ifelse(score > median(score), "hi", "lo"))
  df$w <- rep(c(1, 3), 100)
  df
}

test_that("awkward predictor factor levels work", {
  skip_if_not_installed("nnet")

  dfs <- list(
    unused = transform(
      multinom_df(),
      f = factor(f, levels = c("a", "b", "c", "unused"))
    ),
    colon = multinom_df(\(g) paste0(g, ":1")),
    # levels named after the other predictors in the data
    colliding = multinom_df(\(g) c(a = "x", b = "z", c = "q")[g])
  )

  for (df in dfs) {
    model <- nnet::multinom(y ~ x + z + f, data = df, trace = FALSE)
    probs <- sapply(tidypredict_fit(model), \(f) rlang::eval_tidy(f, df))
    expect_equal(
      unname(probs[, 2]),
      unname(predict(model, df, type = "probs"))
    )
  }
})

test_that("an ordered predictor factor is rejected", {
  skip_if_not_installed("nnet")

  df <- multinom_df(ordered = TRUE)
  model <- nnet::multinom(y ~ x + f, data = df, trace = FALSE)

  expect_snapshot(error = TRUE, tidypredict_fit(model))
})

test_that("an unused outcome level is dropped", {
  skip_if_not_installed("nnet")

  df <- multinom_df()
  df$y <- factor(df$y, levels = c("hi", "lo", "never"))
  model <- nnet::multinom(y ~ x + z, data = df, trace = FALSE)

  tf <- tidypredict_fit(model)
  expect_named(tf, c("hi", "lo"))

  probs <- sapply(tf, \(f) rlang::eval_tidy(f, df))
  expect_equal(unname(probs[, 2]), unname(predict(model, df, type = "probs")))
})

test_that("`NA` in newdata gives the same answer as predict()", {
  skip_if_not_installed("nnet")

  df <- multinom_df()
  na_df <- df
  na_df$x[c(2, 5)] <- NA
  na_df$f[c(1, 3)] <- NA

  binary <- nnet::multinom(y ~ x + z + f, data = df, trace = FALSE)
  probs <- sapply(tidypredict_fit(binary), \(f) rlang::eval_tidy(f, na_df))
  expect_equal(
    unname(probs[, 2]),
    unname(predict(binary, na_df, type = "probs"))
  )

  na_iris <- iris
  na_iris$Petal.Length[1:2] <- NA
  multi <- nnet::multinom(Species ~ ., data = iris, trace = FALSE)
  probs <- sapply(tidypredict_fit(multi), \(f) rlang::eval_tidy(f, na_iris))
  expect_equal(
    unname(probs),
    unname(predict(multi, na_iris, type = "probs"))
  )
})

test_that("`NA` in the training data works", {
  skip_if_not_installed("nnet")

  df <- multinom_df()
  train <- df
  train$x[1:5] <- NA
  model <- nnet::multinom(y ~ x + z, data = train, trace = FALSE)

  probs <- sapply(tidypredict_fit(model), \(f) rlang::eval_tidy(f, df))
  expect_equal(unname(probs[, 2]), unname(predict(model, df, type = "probs")))
})

test_that("weights and decay are reflected in the formula", {
  skip_if_not_installed("nnet")

  df <- multinom_df()
  models <- list(
    nnet::multinom(y ~ x + z, data = df, weights = w, trace = FALSE),
    nnet::multinom(y ~ x + z, data = df, decay = 0.5, trace = FALSE)
  )

  for (model in models) {
    probs <- sapply(tidypredict_fit(model), \(f) rlang::eval_tidy(f, df))
    expect_equal(unname(probs[, 2]), unname(predict(model, df, type = "probs")))
  }
})

test_that("an intercept-only fit works", {
  skip_if_not_installed("nnet")

  df <- multinom_df()
  model <- nnet::multinom(y ~ 1, data = df, trace = FALSE)

  # Every class probability is a constant, so each formula evaluates to length 1
  probs <- sapply(tidypredict_fit(model), \(f) rlang::eval_tidy(f, df))
  expect_equal(
    rep(unname(probs[["lo"]]), nrow(df)),
    unname(predict(model, df, type = "probs"))
  )
})

test_that("model can be saved and re-loaded", {
  skip_if_not_installed("nnet")
  skip_if_not_installed("yaml")

  model <- nnet::multinom(Species ~ ., data = iris, trace = FALSE)

  pm <- parse_model(model)
  mp <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(pm, mp)
  pm <- as_parsed_model(yaml::read_yaml(mp))

  from_model <- sapply(tidypredict_fit(model), \(f) rlang::eval_tidy(f, iris))
  from_pm <- sapply(tidypredict_fit(pm), \(f) rlang::eval_tidy(f, iris))

  expect_equal(from_model, from_pm, tolerance = 1e-6)
})

test_that("tidypredict_test errors for multinom models", {
  skip_if_not_installed("nnet")

  model <- nnet::multinom(Species ~ ., data = iris, trace = FALSE)

  expect_snapshot(error = TRUE, tidypredict_test(model, iris))
})

test_that("inline functions in the formula are rejected", {
  skip_if_not_installed("nnet")

  model <- nnet::multinom(
    Species ~ log(Sepal.Width),
    data = iris,
    trace = FALSE
  )

  expect_snapshot(error = TRUE, tidypredict_fit(model))
})

test_that("SQL translation works", {
  skip_if_not_installed("nnet")
  skip_if_not_installed("dbplyr")

  model <- nnet::multinom(Species ~ ., data = iris, trace = FALSE)

  sql <- tidypredict_sql(model, dbplyr::simulate_dbi())

  expect_named(sql, levels(iris$Species))
  expect_true(all(vapply(sql, \(x) inherits(x, "sql"), logical(1))))
})

test_that("multinom is handled with parsnip", {
  skip_if_not_installed("nnet")
  skip_if_not_installed("parsnip")

  spec <- parsnip::multinom_reg(engine = "nnet")
  model <- parsnip::fit(spec, Species ~ ., iris)

  tf <- tidypredict_fit(model)

  expect_type(tf, "list")
  expect_named(tf, levels(iris$Species))

  probs <- sapply(tf, \(f) rlang::eval_tidy(f, iris))
  native <- as.matrix(predict(model, iris, type = "prob"))

  expect_equal(unname(probs), unname(native))
})

test_that("a coefficient label colliding with a variable name works (#376)", {
  skip_if_not_installed("nnet")

  set.seed(1)
  df <- data.frame(
    g = factor(rep(c("x1", "y2", "z3"), length.out = 60)),
    gy2 = rnorm(60)
  )
  df$y <- factor(ifelse(rnorm(60) + df$gy2 + as.numeric(df$g) > 2, "a", "b"))

  model <- nnet::multinom(y ~ g + gy2, data = df, trace = FALSE)
  probs <- rlang::eval_tidy(tidypredict_fit(model)[["b"]], df)

  expect_equal(probs, unname(predict(model, df, type = "probs")))
})
