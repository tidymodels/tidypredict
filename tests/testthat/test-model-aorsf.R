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
  yaml::write_yaml(parse_model(model), tmp)
  reloaded <- as_parsed_model(yaml::read_yaml(tmp))

  nd <- new_data()
  base <- as.numeric(predict(model, new_data = nd))
  parsed <- rlang::eval_tidy(tidypredict_fit(reloaded), nd)
  expect_equal(parsed, base, tolerance = 1e-6)
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
