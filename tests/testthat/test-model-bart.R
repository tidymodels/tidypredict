bart_data <- function(n = 60) {
  d <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    f2 = factor(sample(c("a", "b"), n, TRUE)),
    f3 = factor(sample(c("p", "q", "r"), n, TRUE))
  )
  d$y <- 2 * d$x1 - d$x2 + as.integer(d$f3) + rnorm(n)
  d
}

bart_fit <- function(df, cols = c("x1", "x2", "f2", "f3"), ...) {
  args <- list(
    keeptrees = TRUE,
    verbose = FALSE,
    nchain = 1,
    nthread = 1,
    ntree = 2,
    ndpost = 2,
    nskip = 10
  )
  args <- utils::modifyList(args, list(...))
  do.call(dbarts::bart, c(list(df[cols], df$y), args))
}

bart_round <- function(pm) {
  pm$general$y_center <- round(pm$general$y_center, 3)
  pm$general$y_scale <- round(pm$general$y_scale, 3)
  pm$trees <- map(pm$trees, function(tree) {
    map(tree, function(node) {
      node$value <- round(node$value, 3)
      node
    })
  })
  pm
}

test_that("returns the right output", {
  skip_if_not_installed("dbarts")

  set.seed(100)
  df <- bart_data()
  model <- bart_fit(df, cols = c("x1", "f2"))

  tf <- tidypredict_fit(model)
  expect_type(tf, "language")

  pm <- parse_model(model)
  expect_s3_class(pm, "list")
  expect_equal(length(pm), 2)
  expect_equal(pm$general$model, "bart")
  expect_equal(pm$general$version, 3)
  expect_equal(pm$general$type, "bart")

  # The leaf values are not stable enough across platforms to be snapshot
  expect_snapshot(rlang::expr_text(tidypredict_fit(bart_round(pm))))
})

test_that("predictions match native predict", {
  skip_if_not_installed("dbarts")

  set.seed(101)
  df <- bart_data()
  model <- bart_fit(df)

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), df),
    colMeans(predict(model, df))
  )
  expect_false(tidypredict_test(model, df)$alert)
})

test_that("predictions match at dbarts' default `ntree` (#305)", {
  skip_if_not_installed("dbarts")

  set.seed(104)
  df <- bart_data(n = 40)
  model <- bart_fit(df, ntree = 200, ndpost = 50)

  # 10,000 leaf values, well over the point at which the terms are summed in a
  # balanced shape rather than from the left. A left fold of them nests deeper
  # than R will evaluate.
  pm <- parse_model(model)
  expect_gt(length(pm$trees), addition_balance_at)

  tf <- tidypredict_fit(model)
  expect_lt(expr_depth(tf), 100)

  expect_equal(
    rlang::eval_tidy(tf, df),
    colMeans(predict(model, df))
  )
})

test_that("predictions match with a matrix of predictors", {
  skip_if_not_installed("dbarts")

  set.seed(102)
  df <- bart_data()
  x <- as.matrix(df[c("x1", "x2")])
  model <- dbarts::bart(
    x,
    df$y,
    keeptrees = TRUE,
    verbose = FALSE,
    nchain = 1,
    nthread = 1,
    ntree = 3,
    ndpost = 3,
    nskip = 10
  )

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), df),
    colMeans(predict(model, x))
  )
})

test_that("predictions match with character predictors", {
  skip_if_not_installed("dbarts")

  set.seed(103)
  df <- bart_data()
  df$f3 <- as.character(df$f3)
  model <- bart_fit(df, cols = c("x1", "f3"))

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), df),
    colMeans(predict(model, df))
  )
})

test_that("predictions match with multiple chains and thinning", {
  skip_if_not_installed("dbarts")

  set.seed(104)
  df <- bart_data()
  model <- bart_fit(df, ndpost = 6, nchain = 3, keepevery = 2)

  expect_equal(.extract_bart_scaling(model)$n_draws, nrow(model$yhat.train))
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), df),
    colMeans(predict(model, df))
  )
})

test_that("predictions match for tree and prior arguments", {
  skip_if_not_installed("dbarts")

  set.seed(105)
  df <- bart_data()

  args <- list(
    list(ntree = 10),
    list(ndpost = 8),
    list(k = 3, power = 3, base = 0.8),
    list(sigdf = 5, sigquant = 0.75, sigest = 1),
    list(usequants = TRUE, numcut = 5),
    list(weights = runif(nrow(df))),
    list(x.test = df[c("x1", "x2", "f2", "f3")])
  )

  for (arg in args) {
    model <- do.call(bart_fit, c(list(df), arg))
    expect_equal(
      rlang::eval_tidy(tidypredict_fit(model), df),
      colMeans(predict(model, df))
    )
  }
})

test_that("produced case_when uses .default", {
  skip_if_not_installed("dbarts")

  set.seed(106)
  df <- bart_data()
  model <- bart_fit(df, ntree = 5, ndpost = 5)

  expect_match(rlang::expr_text(tidypredict_fit(model)), "\\.default")
})

test_that("stumps are supported", {
  skip_if_not_installed("dbarts")

  set.seed(107)
  df <- bart_data()
  # A tree that never splits predicts the same value for every row
  model <- bart_fit(df, ntree = 1, ndpost = 1, nskip = 0, base = 0.000000001)

  fit <- rlang::eval_tidy(tidypredict_fit(model), df)
  preds <- colMeans(predict(model, df))

  expect_length(fit, 1)
  expect_equal(rep(fit, length(preds)), preds)
})

test_that("model can be saved and re-loaded", {
  skip_if_not_installed("dbarts")
  skip_if_not_installed("yaml")

  set.seed(108)
  df <- bart_data()
  model <- bart_fit(df)

  pm <- parse_model(model)
  mp <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(pm, mp)
  pm <- as_parsed_model(yaml::read_yaml(mp))

  # `yaml` rounds the leaf values, so the predictions are not exact
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(pm), df),
    colMeans(predict(model, df)),
    tolerance = 0.000001
  )
})

test_that("SQL translation works", {
  skip_if_not_installed("dbarts")
  skip_if_not_installed("dbplyr")

  set.seed(109)
  df <- bart_data()
  model <- bart_fit(df)

  expect_s3_class(tidypredict_sql(model, dbplyr::simulate_dbi()), "sql")
})

test_that("SQL predictions match in a database", {
  skip_if_not_installed("dbarts")
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("dbplyr")

  set.seed(110)
  df <- bart_data()
  model <- bart_fit(df)

  con <- DBI::dbConnect(RSQLite::SQLite(), path = ":memory:")
  on.exit(DBI::dbDisconnect(con))
  db <- dplyr::copy_to(con, df, "bart_df")

  res <- dplyr::pull(tidypredict_to_column(db, model), fit)

  expect_equal(res, colMeans(predict(model, df)), tolerance = 0.000001)
})

test_that("classification models are not supported", {
  skip_if_not_installed("dbarts")

  set.seed(111)
  df <- bart_data()
  df$y <- factor(ifelse(df$y > 0, "yes", "no"))
  model <- bart_fit(df, cols = c("x1", "x2"))

  expect_snapshot(error = TRUE, tidypredict_fit(model))
  expect_snapshot(error = TRUE, parse_model(model))
})

test_that("models fit without trees are not supported", {
  skip_if_not_installed("dbarts")

  set.seed(112)
  df <- bart_data()
  model <- dbarts::bart(
    df[c("x1", "x2")],
    df$y,
    verbose = FALSE,
    nchain = 1,
    nthread = 1,
    ntree = 2,
    ndpost = 2,
    nskip = 10
  )

  expect_snapshot(error = TRUE, tidypredict_fit(model))
})

test_that("models fit on unnamed predictors are not supported", {
  skip_if_not_installed("dbarts")

  set.seed(113)
  df <- bart_data()
  x <- as.matrix(df[c("x1", "x2")])
  colnames(x) <- NULL
  model <- dbarts::bart(
    x,
    df$y,
    keeptrees = TRUE,
    verbose = FALSE,
    nchain = 1,
    nthread = 1,
    ntree = 2,
    ndpost = 2,
    nskip = 10
  )

  expect_snapshot(error = TRUE, tidypredict_fit(model))
})

test_that("awkward factor level names match predict", {
  skip_if_not_installed("dbarts")

  # An unused level, a level holding a `:`, and a level whose name is also a
  # column in the data all break a parser that splits level names by hand.
  set.seed(200)
  df <- bart_data()
  df$f3 <- factor(
    sample(c("a:b", "x1", "c d"), nrow(df), TRUE),
    levels = c("a:b", "x1", "c d", "unused")
  )
  model <- bart_fit(df, cols = c("x1", "f3"))

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), df),
    colMeans(predict(model, df))
  )
})

test_that("ordered factor predictors match predict", {
  skip_if_not_installed("dbarts")

  set.seed(201)
  df <- bart_data()
  df$f3 <- factor(
    as.character(df$f3),
    levels = c("p", "q", "r"),
    ordered = TRUE
  )
  model <- bart_fit(df, cols = c("x1", "f3"))

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), df),
    colMeans(predict(model, df))
  )
})

test_that("newdata containing NA matches predict on the rows it keeps", {
  skip_if_not_installed("dbarts")

  set.seed(202)
  df <- bart_data()
  model <- bart_fit(df, cols = c("x1", "x2"))

  nd <- df
  nd$x1[1:5] <- NA_real_

  # `dbarts` drops incomplete rows rather than scoring them, so it returns no
  # value for the blanked rows to compare against.
  reference <- colMeans(predict(model, nd))
  expect_length(reference, nrow(df) - 5)

  fit <- rlang::eval_tidy(tidypredict_fit(model), nd)
  expect_length(fit, nrow(df))
  expect_equal(fit[-(1:5)], unname(reference))
})

test_that("a constant outcome and a single predictor match predict", {
  skip_if_not_installed("dbarts")

  set.seed(203)
  df <- bart_data()

  # `dbarts` warns about the perfect fit it gets on a constant outcome, and
  # every tree is a stump, so the formula collapses to a scalar.
  constant <- suppressWarnings(bart_fit(
    transform(df, y = 5),
    cols = c("x1", "x2")
  ))
  fit <- rlang::eval_tidy(tidypredict_fit(constant), df)
  expect_length(fit, 1)
  expect_equal(rep(fit, nrow(df)), unname(colMeans(predict(constant, df))))

  single_column <- bart_fit(df, cols = "x1")
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(single_column), df),
    colMeans(predict(single_column, df))
  )
})

# Tests for .extract_bart_trees() and .extract_bart_scaling() ------------------

test_that(".extract_bart_trees() returns the trees of every draw", {
  skip_if_not_installed("dbarts")

  set.seed(114)
  df <- bart_data()
  model <- bart_fit(df, ntree = 3, ndpost = 4)

  trees <- .extract_bart_trees(model)
  scaling <- .extract_bart_scaling(model)

  expect_type(trees, "list")
  expect_length(trees, 12)
  expect_true(all(vapply(trees, is.language, logical(1))))

  expect_equal(scaling$n_draws, 4)
  expect_equal(scaling$y_center, mean(range(df$y)))
  expect_equal(scaling$y_scale, diff(range(df$y)))

  preds <- map(trees, function(tree) rlang::eval_tidy(tree, df))
  preds <- Reduce(`+`, preds) / scaling$n_draws
  expect_equal(
    preds * scaling$y_scale + scaling$y_center,
    colMeans(predict(model, df))
  )
})

test_that(".extract_bart_trees() errors on the wrong model", {
  skip_if_not_installed("dbarts")

  expect_snapshot(error = TRUE, .extract_bart_trees(lm(mpg ~ wt, mtcars)))
  expect_snapshot(error = TRUE, .extract_bart_scaling(lm(mpg ~ wt, mtcars)))
})
