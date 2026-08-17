test_that("returns the right output", {
  skip_if_not_installed("mboost")

  set.seed(1)
  model <- mboost::blackboost(
    mpg ~ wt + cyl,
    data = mtcars,
    control = mboost::boost_control(mstop = 10)
  )

  tf <- tidypredict_fit(model)
  expect_type(tf, "language")

  pm <- parse_model(model)
  expect_s3_class(pm, "list")
  expect_equal(length(pm), 2)
  expect_equal(pm$general$model, "blackboost")
  expect_equal(pm$general$version, 3)
})

test_that("blackboost regression predictions match", {
  skip_if_not_installed("mboost")

  set.seed(1)
  model <- mboost::blackboost(
    mpg ~ wt + cyl,
    data = mtcars,
    control = mboost::boost_control(mstop = 10)
  )

  fit <- rlang::eval_tidy(tidypredict_fit(model), mtcars)
  expect_equal(fit, as.numeric(predict(model, newdata = mtcars)))
  expect_false(tidypredict_test(model, df = mtcars)$alert)
})

test_that("blackboost works with categorical predictors", {
  skip_if_not_installed("mboost")

  set.seed(1)
  df <- transform(mtcars, cyl = factor(cyl))
  model <- mboost::blackboost(
    mpg ~ wt + cyl,
    data = df,
    control = mboost::boost_control(mstop = 10)
  )

  expect_false(tidypredict_test(model, df = df)$alert)
})

mboost_factor_df <- function(levels_of = identity, ordered = FALSE) {
  set.seed(1)
  df <- data.frame(x = rnorm(200), z = rnorm(200))
  g <- rep(c("a", "b", "c"), length.out = 200)
  df$f <- factor(levels_of(g), ordered = ordered)
  df$y <- rnorm(200) + df$x + as.numeric(factor(g))
  df
}

test_that("awkward factor levels work", {
  skip_if_not_installed("mboost")

  dfs <- list(
    unused = transform(
      mboost_factor_df(),
      f = factor(f, levels = c("a", "b", "c", "unused"))
    ),
    ordered = mboost_factor_df(ordered = TRUE),
    colon = mboost_factor_df(\(g) paste0(g, ":1")),
    # levels named after the other predictors in the data
    colliding = mboost_factor_df(\(g) c(a = "x", b = "z", c = "q")[g])
  )

  for (df in dfs) {
    model <- mboost::blackboost(
      y ~ x + z + f,
      data = df,
      control = mboost::boost_control(mstop = 10)
    )
    expect_equal(
      rlang::eval_tidy(tidypredict_fit(model), df),
      as.numeric(predict(model, df))
    )
  }
})

test_that("a value exactly at a split threshold lands in the same leaf", {
  skip_if_not_installed("mboost")

  set.seed(1)
  df <- data.frame(x = rnorm(200), z = rnorm(200))
  df$y <- rnorm(200) + df$x
  model <- mboost::blackboost(
    y ~ x + z,
    data = df,
    control = mboost::boost_control(mstop = 10)
  )

  fit <- tidypredict_fit(model)
  thresholds <- as.numeric(unique(regmatches(
    rlang::expr_text(fit),
    gregexpr("(?<=x <= )[-0-9.e]+", rlang::expr_text(fit), perl = TRUE)
  )[[1]]))
  expect_gt(length(thresholds), 0)

  # exactly at the threshold, and one ulp either side of it
  nd <- data.frame(
    x = c(thresholds, thresholds - 1e-12, thresholds + 1e-12),
    z = 0
  )

  expect_equal(
    rlang::eval_tidy(fit, nd),
    as.numeric(predict(model, nd))
  )
})

test_that("`NA` in the training data works", {
  skip_if_not_installed("mboost")

  set.seed(1)
  df <- data.frame(x = rnorm(200), z = rnorm(200))
  df$y <- rnorm(200) + df$x
  train <- df
  train$x[1:10] <- NA

  model <- mboost::blackboost(
    y ~ x + z,
    data = train,
    control = mboost::boost_control(mstop = 10)
  )

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), df),
    as.numeric(predict(model, df))
  )
})

test_that("degenerate fit shapes work", {
  skip_if_not_installed("mboost")
  skip_if_not_installed("partykit")

  set.seed(1)
  df <- data.frame(x = rnorm(200), z = rnorm(200))
  df$y <- rnorm(200) + df$x

  stump <- mboost::blackboost(
    y ~ x + z,
    data = df,
    control = mboost::boost_control(mstop = 5),
    tree_controls = partykit::ctree_control(maxdepth = 1, mincriterion = 0)
  )
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(stump), df),
    as.numeric(predict(stump, df))
  )

  one_step <- mboost::blackboost(
    y ~ x + z,
    data = df,
    control = mboost::boost_control(mstop = 1)
  )
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(one_step), df),
    as.numeric(predict(one_step, df))
  )

  # A constant outcome leaves every tree root-only, so the formula is a constant
  noise <- df
  noise$y <- 5
  root_only <- mboost::blackboost(
    y ~ x + z,
    data = noise,
    control = mboost::boost_control(mstop = 5)
  )
  expect_equal(
    rep(rlang::eval_tidy(tidypredict_fit(root_only), noise), nrow(noise)),
    as.numeric(predict(root_only, noise))
  )

  single_row <- mboost::blackboost(
    y ~ x + z,
    data = df[1, ],
    control = mboost::boost_control(mstop = 5)
  )
  expect_equal(
    rep(rlang::eval_tidy(tidypredict_fit(single_row), df), nrow(df)),
    as.numeric(predict(single_row, df))
  )
})

test_that("blackboost respects mstop and nu", {
  skip_if_not_installed("mboost")

  set.seed(1)
  model <- mboost::blackboost(
    mpg ~ wt + cyl + disp,
    data = mtcars,
    control = mboost::boost_control(mstop = 50, nu = 0.3)
  )

  fit <- rlang::eval_tidy(tidypredict_fit(model), mtcars)
  expect_equal(fit, as.numeric(predict(model, newdata = mtcars)))
})

test_that("blackboost honours an mstop reduced after fitting (#306)", {
  skip_if_not_installed("mboost")

  # The `cvrisk()` workflow subsets a fitted model to the best iteration, which
  # sets `mstop` but leaves the stored ensemble at its full length.
  set.seed(1)
  model <- mboost::blackboost(
    mpg ~ wt + cyl + disp,
    data = mtcars,
    control = mboost::boost_control(mstop = 100)
  )
  model[30]
  expect_equal(mboost::mstop(model), 30)

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), mtcars),
    as.numeric(predict(model, newdata = mtcars))
  )
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(parse_model(model)), mtcars),
    as.numeric(predict(model, newdata = mtcars))
  )
})

test_that("blackboost supports SQL", {
  skip_if_not_installed("mboost")

  set.seed(1)
  model <- mboost::blackboost(
    mpg ~ wt + cyl,
    data = mtcars,
    control = mboost::boost_control(mstop = 5)
  )

  expect_s3_class(tidypredict_sql(model, dbplyr::simulate_dbi()), "sql")
})

test_that("parse_model roundtrips and produces correct predictions", {
  skip_if_not_installed("mboost")

  set.seed(1)
  model <- mboost::blackboost(
    mpg ~ wt + cyl,
    data = mtcars,
    control = mboost::boost_control(mstop = 10)
  )

  pm <- parse_model(model)
  expect_s3_class(pm, "pm_tree")
  expect_identical(tidypredict_fit(pm), tidypredict_fit(model))

  base <- as.numeric(predict(model, newdata = mtcars))
  parsed <- rlang::eval_tidy(tidypredict_fit(pm), mtcars)
  expect_equal(parsed, base)
})

test_that("model can be saved and re-loaded", {
  skip_if_not_installed("mboost")
  skip_if_not_installed("yaml")

  set.seed(1)
  model <- mboost::blackboost(
    mpg ~ wt + cyl,
    data = mtcars,
    control = mboost::boost_control(mstop = 10)
  )

  tmp <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(parse_model(model), tmp)
  reloaded <- as_parsed_model(yaml::read_yaml(tmp))

  base <- as.numeric(predict(model, newdata = mtcars))
  parsed <- rlang::eval_tidy(tidypredict_fit(reloaded), mtcars)
  expect_equal(parsed, base, tolerance = 1e-6)
})

test_that("non-Gaussian families error with clear message", {
  skip_if_not_installed("mboost")

  set.seed(1)
  model <- mboost::blackboost(
    factor(vs) ~ wt + cyl,
    data = mtcars,
    family = mboost::Binomial(),
    control = mboost::boost_control(mstop = 10)
  )

  expect_snapshot(error = TRUE, tidypredict_fit(model))
  expect_snapshot(error = TRUE, parse_model(model))
})

test_that("a missing predictor gives NA rather than a random draw (#294)", {
  skip_if_not_installed("mboost")

  set.seed(1)
  model <- mboost::blackboost(mpg ~ wt + disp + cyl, data = mtcars)

  df <- mtcars
  df$wt[1:4] <- NA_real_
  fit <- rlang::eval_tidy(tidypredict_fit(model), df)

  expect_length(fit, nrow(df))
  expect_true(all(is.na(fit[1:4])))
  expect_equal(
    fit[-(1:4)],
    as.numeric(predict(model, mtcars))[-(1:4)]
  )
})

test_that("a missing factor level also gives NA (#294)", {
  skip_if_not_installed("mboost")

  # `predict()` sends a missing factor down a surrogate split and returns a
  # number; the generated formula deliberately propagates the `NA` instead.
  set.seed(1)
  df <- data.frame(x = rnorm(200))
  df$f <- factor(rep(c("a", "b", "c"), length.out = 200))
  df$y <- rnorm(200) + df$x + as.numeric(df$f)
  model <- mboost::blackboost(
    y ~ x + f,
    data = df,
    control = mboost::boost_control(mstop = 10)
  )

  na_df <- df
  na_df$f[1:2] <- NA
  fit <- rlang::eval_tidy(tidypredict_fit(model), na_df)

  expect_true(all(is.na(fit[1:2])))
  expect_equal(fit[-(1:2)], as.numeric(predict(model, df))[-(1:2)])
})
