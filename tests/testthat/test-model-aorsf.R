# aorsf uses observed linear-combination values as split cutpoints, so training
# rows land exactly on a split boundary, where the formula cannot reproduce the
# last bit of aorsf's fused-multiply-add traversal. The split thresholds are
# calibrated against the training data to compensate, so agreement is checked on
# the training rows as well as on new data.

new_data <- function() {
  set.seed(99)
  df <- mtcars
  df[] <- lapply(mtcars, function(x) x + rnorm(length(x), 0, 0.01))
  df
}

test_that("aorsf regression predictions match", {
  skip_if_not_installed("aorsf")

  set.seed(1)
  model <- aorsf::orsf(mtcars, mpg ~ wt + cyl + disp + hp, n_tree = 20)

  expect_type(tidypredict_fit(model), "language")

  nd <- new_data()
  fit <- rlang::eval_tidy(tidypredict_fit(model), nd)
  base <- as.numeric(predict(model, new_data = nd))
  expect_equal(fit, base)
  expect_false(tidypredict_test(model, df = nd)$alert)
})

test_that("aorsf predictions match on the training data (#351)", {
  skip_if_not_installed("aorsf")

  set.seed(1)
  model <- aorsf::orsf(mtcars, mpg ~ wt + cyl + disp + hp, n_tree = 20)

  fit <- rlang::eval_tidy(tidypredict_fit(model), mtcars)
  base <- as.numeric(predict(model, new_data = mtcars))
  expect_equal(fit, base)
  expect_false(tidypredict_test(model, df = mtcars)$alert)
})

test_that("training data agreement holds across forest sizes (#351)", {
  skip_if_not_installed("aorsf")

  set.seed(3)
  df <- as.data.frame(matrix(rnorm(150 * 5), 150, 5))
  names(df) <- paste0("x", 1:5)
  df$y <- rowSums(df) + rnorm(150)

  for (n_tree in c(1, 5, 50)) {
    for (vars in list(c("x1", "x2"), paste0("x", 1:5))) {
      f <- stats::reformulate(vars, "y")
      model <- aorsf::orsf(df[c(vars, "y")], f, n_tree = n_tree)
      fit <- rlang::eval_tidy(tidypredict_fit(model), df)
      expect_equal(fit, as.numeric(predict(model, new_data = df)))
    }
  }
})

test_that("aorsf supports SQL", {
  skip_if_not_installed("aorsf")

  set.seed(1)
  model <- aorsf::orsf(mtcars, mpg ~ wt + cyl + disp, n_tree = 10)

  expect_s3_class(tidypredict_sql(model, dbplyr::simulate_dbi()), "sql")
})

test_that("aorsf predictions round-trip through SQLite", {
  skip_if_not_installed("aorsf")
  skip_if_not_installed("RSQLite")

  set.seed(1)
  model <- aorsf::orsf(mtcars, mpg ~ wt + cyl + disp, n_tree = 10)

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con))
  nd <- new_data()
  DBI::dbWriteTable(con, "mtcars", nd)

  sql_res <- dplyr::tbl(con, "mtcars") |>
    dplyr::mutate(pred = !!tidypredict_fit(model)) |>
    dplyr::pull(pred)
  r_res <- rlang::eval_tidy(tidypredict_fit(model), nd)

  expect_equal(sql_res, r_res)
})

test_that("parse_model roundtrips and produces correct predictions", {
  skip_if_not_installed("aorsf")

  set.seed(1)
  model <- aorsf::orsf(mtcars, mpg ~ wt + cyl + disp, n_tree = 10)

  pm <- parse_model(model)
  expect_s3_class(pm, "pm_tree")
  expect_equal(pm$general$model, "aorsf")
  expect_identical(tidypredict_fit(pm), tidypredict_fit(model))

  nd <- new_data()
  base <- as.numeric(predict(model, new_data = nd))
  parsed <- rlang::eval_tidy(tidypredict_fit(pm), nd)
  expect_equal(parsed, base)

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(pm), mtcars),
    as.numeric(predict(model, new_data = mtcars))
  )
})

test_that("model can be saved and re-loaded", {
  skip_if_not_installed("aorsf")
  skip_if_not_installed("yaml")

  set.seed(1)
  model <- aorsf::orsf(mtcars, mpg ~ wt + cyl + disp, n_tree = 10)

  tmp <- withr::local_tempfile(fileext = ".yml")
  tidypredict_save(model, tmp)
  reloaded <- tidypredict_load(tmp)

  nd <- new_data()
  base <- as.numeric(predict(model, new_data = nd))
  parsed <- rlang::eval_tidy(tidypredict_fit(reloaded), nd)
  expect_equal(parsed, base)

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(reloaded), mtcars),
    as.numeric(predict(model, new_data = mtcars))
  )
})

test_that("classification errors with clear message", {
  skip_if_not_installed("aorsf")

  set.seed(1)
  d <- mtcars
  d$am <- factor(d$am)
  model <- aorsf::orsf(d, am ~ wt + mpg + disp, n_tree = 5)

  expect_snapshot(error = TRUE, tidypredict_fit(model))
  expect_snapshot(error = TRUE, parse_model(model))
})

test_that("non-numeric predictors error with clear message", {
  skip_if_not_installed("aorsf")

  set.seed(1)
  d <- mtcars
  d$cyl <- factor(d$cyl)
  model <- aorsf::orsf(d, mpg ~ wt + cyl, n_tree = 5)

  expect_snapshot(error = TRUE, tidypredict_fit(model))
})

test_that("a missing predictor gives NA rather than a confident value (#325)", {
  skip_if_not_installed("aorsf")

  set.seed(1)
  model <- aorsf::orsf(mtcars, mpg ~ wt + cyl + disp + hp, n_tree = 20)

  nd <- new_data()
  nd$wt[1:5] <- NA_real_
  fit <- rlang::eval_tidy(tidypredict_fit(model), nd)

  # `predict()` refuses the incomplete rows outright, so there is no value to
  # match; the rows are kept rather than dropped.
  expect_error(predict(model, new_data = nd), "missing values")
  expect_length(fit, nrow(nd))
  expect_equal(is.na(fit), c(rep(TRUE, 5), rep(FALSE, nrow(nd) - 5)))

  # The complete rows are unaffected.
  expect_equal(
    fit[-(1:5)],
    as.numeric(predict(model, new_data = nd[-(1:5), ]))
  )
})
