# aorsf uses observed linear-combination values as split cutpoints, so training
# rows can land exactly on a split boundary where floating-point drift between
# aorsf's C++ traversal and the generated formula flips the branch. These tests
# therefore evaluate agreement on jittered data, where such exact ties do not
# occur. On genuinely new data the formula reproduces `predict()` exactly.

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

test_that("non-default control and sampling arguments match predict()", {
  skip_if_not_installed("aorsf")

  nd <- new_data()

  set.seed(1)
  net <- aorsf::orsf(
    mtcars,
    mpg ~ wt + cyl + disp + hp,
    n_tree = 10,
    control = aorsf::orsf_control_regression(method = "net")
  )
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(net), nd),
    as.numeric(predict(net, new_data = nd))
  )

  set.seed(1)
  tuned <- aorsf::orsf(
    mtcars,
    mpg ~ wt + cyl + disp + hp,
    n_tree = 10,
    n_split = 1,
    leaf_min_obs = 10,
    mtry = 2
  )
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(tuned), nd),
    as.numeric(predict(tuned, new_data = nd))
  )

  set.seed(1)
  subsampled <- aorsf::orsf(
    mtcars,
    mpg ~ wt + cyl + disp,
    n_tree = 10,
    sample_fraction = 0.5,
    sample_with_replacement = FALSE
  )
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(subsampled), nd),
    as.numeric(predict(subsampled, new_data = nd))
  )
})

test_that("degenerate forests match predict()", {
  skip_if_not_installed("aorsf")

  nd <- new_data()

  # `split_min_obs` just under the row count leaves every tree a single split.
  set.seed(1)
  shallow <- aorsf::orsf(
    mtcars,
    mpg ~ wt + cyl + disp,
    n_tree = 10,
    split_min_obs = 31
  )
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(shallow), nd),
    as.numeric(predict(shallow, new_data = nd))
  )

  set.seed(1)
  single <- aorsf::orsf(mtcars, mpg ~ wt, n_tree = 10)
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(single), nd),
    as.numeric(predict(single, new_data = nd))
  )

  set.seed(1)
  tiny <- aorsf::orsf(
    mtcars[1:5, ],
    mpg ~ wt + hp,
    n_tree = 5,
    leaf_min_obs = 1,
    split_min_obs = 2
  )
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(tiny), nd),
    as.numeric(predict(tiny, new_data = nd))
  )

  flat <- transform(mtcars, mpg = 5)
  set.seed(1)
  constant <- aorsf::orsf(flat, mpg ~ wt + hp, n_tree = 5)
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(constant), flat),
    as.numeric(predict(constant, new_data = flat))
  )
})
