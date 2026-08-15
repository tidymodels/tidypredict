nnet_reg_df <- function() {
  df <- mtcars
  df$cyl <- factor(df$cyl)
  df
}

test_that("returns the right output", {
  skip_if_not_installed("nnet")

  # Fixed weights keep the fit, and so the snapshot, stable across platforms
  model <- nnet::nnet(
    mpg ~ wt + hp,
    data = mtcars,
    size = 2,
    linout = TRUE,
    maxit = 0,
    Wts = seq(0.1, 0.9, by = 0.1),
    trace = FALSE
  )

  tf <- tidypredict_fit(model)
  expect_type(tf, "language")

  pm <- parse_model(model)
  expect_s3_class(pm, "list")
  expect_equal(length(pm), 3)
  expect_equal(pm$general$model, "nnet")
  expect_equal(pm$general$version, 2)
  expect_equal(pm$general$type, "nnet")
  expect_equal(pm$general$n_outputs, 1)
  expect_false(pm$general$softmax)

  expect_snapshot(rlang::expr_text(tf))
})

test_that("regression predictions match native predict", {
  skip_if_not_installed("nnet")

  df <- nnet_reg_df()
  set.seed(100)
  model <- nnet::nnet(
    mpg ~ wt + hp + cyl,
    data = df,
    size = 3,
    linout = TRUE,
    trace = FALSE
  )

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), df),
    as.numeric(predict(model, df))
  )
  expect_false(tidypredict_test(model, df)$alert)
})

test_that("logistic output units are handled", {
  skip_if_not_installed("nnet")

  df <- transform(mtcars, drat = drat / max(drat))
  set.seed(100)
  model <- nnet::nnet(drat ~ wt + hp, data = df, size = 2, trace = FALSE)

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), df),
    as.numeric(predict(model, df))
  )
})

test_that("skip layer connections are handled", {
  skip_if_not_installed("nnet")

  df <- nnet_reg_df()
  set.seed(100)
  model <- nnet::nnet(
    mpg ~ wt + hp + cyl,
    data = df,
    size = 2,
    linout = TRUE,
    skip = TRUE,
    trace = FALSE
  )

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), df),
    as.numeric(predict(model, df))
  )
})

test_that("networks without hidden units are handled", {
  skip_if_not_installed("nnet")

  set.seed(100)
  model <- nnet::nnet(
    mpg ~ wt + hp,
    data = mtcars,
    size = 0,
    skip = TRUE,
    linout = TRUE,
    trace = FALSE
  )

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), mtcars),
    as.numeric(predict(model, mtcars))
  )
})

test_that("weight decay and interactions are handled", {
  skip_if_not_installed("nnet")

  set.seed(100)
  model <- nnet::nnet(
    mpg ~ wt * hp,
    data = mtcars,
    size = 2,
    linout = TRUE,
    decay = 0.1,
    trace = FALSE
  )

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), mtcars),
    as.numeric(predict(model, mtcars))
  )
})

test_that("the squashing function saturates the way nnet does", {
  skip_if_not_installed("nnet")

  # `b -> h1 = 0`, `wt -> h1 = 1`, so the hidden unit sees `wt` directly and
  # `nnet()` returns exactly 0 and 1 outside of `[-15, 15]`
  df <- data.frame(wt = c(-20, -15, 0, 15, 20), mpg = 0)
  model <- nnet::nnet(
    mpg ~ wt,
    data = df,
    size = 1,
    linout = TRUE,
    maxit = 0,
    Wts = c(0, 1, 0, 1),
    trace = FALSE
  )

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), df),
    as.numeric(predict(model, df))
  )
})

test_that("multiclass predictions match native predict", {
  skip_if_not_installed("nnet")

  set.seed(100)
  model <- nnet::nnet(Species ~ ., data = iris, size = 3, trace = FALSE)

  tf <- tidypredict_fit(model)
  expect_type(tf, "list")
  expect_named(tf, levels(iris$Species))
  expect_true(all(vapply(tf, is.language, logical(1))))

  probs <- sapply(tf, \(f) rlang::eval_tidy(f, iris))

  expect_equal(unname(probs), unname(predict(model, iris, type = "raw")))
  expect_equal(unname(rowSums(probs)), rep(1, nrow(iris)))
})

test_that("binary outcomes are handled", {
  skip_if_not_installed("nnet")

  df <- transform(mtcars, am = factor(am))
  set.seed(100)
  model <- nnet::nnet(am ~ mpg + wt, data = df, size = 2, trace = FALSE)

  tf <- tidypredict_fit(model)
  expect_named(tf, c("0", "1"))

  probs <- sapply(tf, \(f) rlang::eval_tidy(f, df))

  expect_equal(unname(probs[, 2]), as.numeric(predict(model, df, type = "raw")))
  expect_equal(unname(rowSums(probs)), rep(1, nrow(df)))
})

test_that("model can be saved and re-loaded", {
  skip_if_not_installed("nnet")
  skip_if_not_installed("yaml")

  df <- nnet_reg_df()
  set.seed(100)
  model <- nnet::nnet(
    mpg ~ wt + hp + cyl,
    data = df,
    size = 2,
    linout = TRUE,
    trace = FALSE
  )

  pm <- parse_model(model)
  mp <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(pm, mp)
  pm <- as_parsed_model(yaml::read_yaml(mp))

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(pm), df),
    rlang::eval_tidy(tidypredict_fit(model), df),
    tolerance = 1e-6
  )
})

test_that("multiple non classification outputs are rejected", {
  skip_if_not_installed("nnet")

  y <- cbind(a = mtcars$mpg, b = mtcars$disp)
  model <- nnet::nnet(
    mtcars[, c("wt", "hp")],
    y,
    size = 2,
    linout = TRUE,
    trace = FALSE
  )

  expect_snapshot(error = TRUE, tidypredict_fit(model))
})

test_that("matrix interface fits are rejected", {
  skip_if_not_installed("nnet")

  set.seed(100)
  model <- nnet::nnet(
    as.matrix(mtcars[, c("wt", "hp")]),
    mtcars$mpg,
    size = 2,
    linout = TRUE,
    trace = FALSE
  )

  expect_snapshot(error = TRUE, tidypredict_fit(model))
  expect_snapshot(error = TRUE, parse_model(model))
})

test_that("tidypredict_test errors for classification nnet models", {
  skip_if_not_installed("nnet")

  set.seed(100)
  model <- nnet::nnet(Species ~ ., data = iris, size = 2, trace = FALSE)

  expect_snapshot(error = TRUE, tidypredict_test(model, iris))
})

test_that("inline functions in the formula are rejected", {
  skip_if_not_installed("nnet")

  model <- nnet::nnet(
    mpg ~ log(wt),
    data = mtcars,
    size = 1,
    linout = TRUE,
    maxit = 0,
    trace = FALSE
  )

  expect_snapshot(error = TRUE, tidypredict_fit(model))
})

test_that("SQL translation works", {
  skip_if_not_installed("nnet")
  skip_if_not_installed("dbplyr")

  set.seed(100)
  model <- nnet::nnet(
    mpg ~ wt + hp,
    data = mtcars,
    size = 2,
    linout = TRUE,
    trace = FALSE
  )

  expect_s3_class(tidypredict_sql(model, dbplyr::simulate_dbi()), "sql")
})

test_that("SQL predictions match native predict", {
  skip_if_not_installed("nnet")
  skip_if_not_installed("dbplyr")
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  set.seed(100)
  model <- nnet::nnet(
    mpg ~ wt + hp,
    data = mtcars,
    size = 2,
    linout = TRUE,
    trace = FALSE
  )

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  db <- dplyr::copy_to(con, mtcars, "mtcars")

  res <- dplyr::collect(dplyr::mutate(db, fit = !!tidypredict_fit(model)))

  expect_equal(res$fit, as.numeric(predict(model, mtcars)))
})
