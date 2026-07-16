test_that("returns the right output", {
  skip_if_not_installed("C50")
  df <- mtcars
  df$vs <- factor(df$vs)
  model <- C50::C5.0(df[, c("wt", "cyl", "mpg")], df$vs)

  tf <- tidypredict_fit(model)
  pm <- parse_model(model)

  expect_type(tf, "language")

  expect_s3_class(pm, "list")
  expect_equal(length(pm), 2)
  expect_equal(pm$general$model, "C5.0")
  expect_equal(pm$general$version, 3)

  expect_snapshot(rlang::expr_text(tf))
})

test_that("numeric predictors match predict()", {
  skip_if_not_installed("C50")
  df <- mtcars
  df$vs <- factor(df$vs)
  model <- C50::C5.0(df[, c("wt", "cyl", "mpg")], df$vs)

  fit_pred <- as.character(rlang::eval_tidy(tidypredict_fit(model), df))
  expect_equal(fit_pred, as.character(predict(model, df)))
})

test_that("binary categorical splits match predict()", {
  skip_if_not_installed("C50")
  df <- mtcars
  df$vs <- factor(df$vs)
  df$gear <- factor(df$gear)
  model <- C50::C5.0(df[, c("wt", "gear", "mpg")], df$vs)

  fit_pred <- as.character(rlang::eval_tidy(tidypredict_fit(model), df))
  expect_equal(fit_pred, as.character(predict(model, df)))
})

test_that("multiway categorical splits match predict()", {
  skip_if_not_installed("C50")
  set.seed(3)
  n <- 400
  f <- factor(sample(c("a", "b", "c", "d"), n, TRUE))
  y <- factor(c(a = "p", b = "q", c = "r", d = "p")[as.character(f)])
  df <- data.frame(f = f, y = y)
  model <- C50::C5.0(
    df["f"],
    df$y,
    control = C50::C5.0Control(CF = 0.9, minCases = 1)
  )

  n_in <- lengths(regmatches(
    rlang::expr_text(tidypredict_fit(model)),
    gregexpr("%in%", rlang::expr_text(tidypredict_fit(model)))
  ))
  expect_gt(n_in, 1)
  fit_pred <- as.character(rlang::eval_tidy(tidypredict_fit(model), df))
  expect_equal(fit_pred, as.character(predict(model, df)))
})

test_that("stump trees (no splits) work correctly", {
  skip_if_not_installed("C50")
  df <- mtcars
  y <- factor(c(rep("a", 30), rep("b", 2)))
  model <- C50::C5.0(df[, c("wt", "cyl"), drop = FALSE], y)

  expect_equal(tidypredict_fit(model), "a")
})

test_that("produced case_when uses .default", {
  skip_if_not_installed("C50")
  df <- mtcars
  df$vs <- factor(df$vs)
  model <- C50::C5.0(df[, c("wt", "cyl", "mpg")], df$vs)

  expect_match(rlang::expr_text(tidypredict_fit(model)), "\\.default")
})

test_that("errors on unsupported configurations", {
  skip_if_not_installed("C50")
  df <- mtcars
  df$vs <- factor(df$vs)

  boosted <- C50::C5.0(df[, c("wt", "cyl")], df$vs, trials = 5)
  rules <- C50::C5.0(df[, c("wt", "cyl")], df$vs, rules = TRUE)

  expect_snapshot(tidypredict_fit(boosted), error = TRUE)
  expect_snapshot(tidypredict_fit(rules), error = TRUE)
})

test_that("SQL translation works", {
  skip_if_not_installed("C50")
  df <- mtcars
  df$vs <- factor(df$vs)
  model <- C50::C5.0(df[, c("wt", "cyl", "mpg")], df$vs)

  expect_s3_class(tidypredict_sql(model, dbplyr::simulate_dbi()), "sql")
})

test_that("predictions round-trip through a SQLite database", {
  skip_if_not_installed("C50")
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("dbplyr")

  df <- mtcars
  df$vs <- factor(df$vs)
  model <- C50::C5.0(df[, c("wt", "cyl", "mpg")], df$vs)

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con))
  dplyr::copy_to(con, df, "mt")
  db_pred <- dplyr::tbl(con, "mt") |>
    dplyr::mutate(pred = !!tidypredict_fit(model)) |>
    dplyr::pull(pred)

  expect_equal(as.character(db_pred), as.character(predict(model, df)))
})

test_that("model can be saved and re-loaded", {
  skip_if_not_installed("C50")
  df <- mtcars
  df$vs <- factor(df$vs)
  model <- C50::C5.0(df[, c("wt", "cyl", "mpg")], df$vs)

  pm <- parse_model(model)
  tmp <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(pm, tmp)
  pm2 <- as_parsed_model(yaml::read_yaml(tmp))

  expect_equal(
    rlang::expr_text(tidypredict_fit(pm)),
    rlang::expr_text(tidypredict_fit(pm2))
  )
})

test_that(".c50_tree_info_full is exported and works", {
  skip_if_not_installed("C50")
  df <- mtcars
  df$vs <- factor(df$vs)
  model <- C50::C5.0(df[, c("wt", "cyl", "mpg")], df$vs)

  info <- .c50_tree_info_full(model)
  expect_type(info, "list")
  expect_true(all(
    c("nodeID", "leftChild", "rightChild", "terminal") %in% names(info)
  ))
})
