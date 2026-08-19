test_that("returns the right output", {
  skip_if_not_installed("kernlab")

  set.seed(1)
  model <- kernlab::ksvm(
    mpg ~ wt + hp + disp,
    data = mtcars,
    kernel = "vanilladot",
    type = "eps-svr"
  )

  tf <- tidypredict_fit(model)
  pm <- parse_model(model)

  expect_type(tf, "language")

  expect_s3_class(pm, "list")
  expect_equal(length(pm), 2)
  expect_equal(pm$general$model, "ksvm")
  expect_equal(pm$general$version, 2)
})

test_that("regression predictions match predict()", {
  skip_if_not_installed("kernlab")

  df <- mtcars
  df$gear <- factor(df$gear)

  for (svm_type in c("eps-svr", "nu-svr")) {
    set.seed(1)
    model <- kernlab::ksvm(
      mpg ~ wt + hp + gear,
      data = df,
      kernel = "vanilladot",
      type = svm_type
    )
    te <- rlang::eval_tidy(tidypredict_fit(model), df)
    base <- as.numeric(kernlab::predict(model, df))
    expect_equal(te, base, tolerance = 1e-10)
    expect_false(tidypredict_test(model, df)$alert)
  }
})

test_that("regression predictions match predict() with a single scaled column", {
  skip_if_not_installed("kernlab")

  df <- mtcars
  df$gear <- factor(df$gear)

  for (fo in c(mpg ~ wt, mpg ~ wt + gear)) {
    set.seed(1)
    model <- kernlab::ksvm(
      fo,
      data = df,
      kernel = "vanilladot",
      type = "eps-svr"
    )
    te <- rlang::eval_tidy(tidypredict_fit(model), df)
    base <- as.numeric(kernlab::predict(model, df))
    expect_equal(te, base, tolerance = 1e-10)
    expect_false(tidypredict_test(model, df)$alert)
  }
})

test_that("binary classification probabilities match predict()", {
  skip_if_not_installed("kernlab")

  df <- mtcars
  df$am <- factor(ifelse(df$am == 1, "yes", "no"))
  df$gear <- factor(df$gear)

  for (svm_type in c("C-svc", "nu-svc")) {
    set.seed(1)
    model <- kernlab::ksvm(
      am ~ wt + hp + gear,
      data = df,
      kernel = "vanilladot",
      type = svm_type,
      prob.model = TRUE
    )
    te <- rlang::eval_tidy(tidypredict_fit(model), df)
    base <- kernlab::predict(model, df, type = "probabilities")[, "yes"]
    expect_equal(te, unname(base), tolerance = 1e-10)
    expect_false(tidypredict_test(model, df)$alert)
  }
})

test_that("classification probabilities match predict() with a single scaled column", {
  skip_if_not_installed("kernlab")

  df <- mtcars
  df$am <- factor(ifelse(df$am == 1, "yes", "no"))
  df$gear <- factor(df$gear)

  for (fo in c(am ~ wt, am ~ wt + gear)) {
    set.seed(1)
    model <- kernlab::ksvm(
      fo,
      data = df,
      kernel = "vanilladot",
      type = "C-svc",
      prob.model = TRUE
    )
    te <- rlang::eval_tidy(tidypredict_fit(model), df)
    base <- kernlab::predict(model, df, type = "probabilities")[, "yes"]
    expect_equal(te, unname(base), tolerance = 1e-10)
    expect_false(tidypredict_test(model, df)$alert)
  }
})

test_that("works without predictor scaling", {
  skip_if_not_installed("kernlab")

  set.seed(1)
  model <- kernlab::ksvm(
    mpg ~ wt + hp,
    data = mtcars,
    kernel = "vanilladot",
    type = "eps-svr",
    scaled = FALSE
  )
  expect_type(tidypredict_fit(model), "language")
  expect_false(tidypredict_test(model, mtcars)$alert)
})

test_that("model can be saved and re-loaded", {
  skip_if_not_installed("yaml")
  skip_if_not_installed("kernlab")

  set.seed(1)
  model <- kernlab::ksvm(
    mpg ~ wt + hp,
    data = mtcars,
    kernel = "vanilladot",
    type = "eps-svr"
  )

  pm <- parse_model(model)
  mp <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(pm, mp, precision = 22)
  pm <- as_parsed_model(yaml::read_yaml(mp))

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), mtcars),
    rlang::eval_tidy(tidypredict_fit(pm), mtcars),
    tolerance = 1e-10
  )
})

test_that("errors on unsupported models", {
  skip_if_not_installed("kernlab")

  set.seed(1)
  rbf <- kernlab::ksvm(
    mpg ~ wt,
    data = mtcars,
    kernel = "rbfdot",
    type = "eps-svr"
  )
  expect_snapshot(tidypredict_fit(rbf), error = TRUE)

  set.seed(1)
  multi <- kernlab::ksvm(
    Species ~ .,
    data = iris,
    kernel = "vanilladot",
    type = "C-svc",
    prob.model = TRUE
  )
  expect_snapshot(tidypredict_fit(multi), error = TRUE)

  set.seed(1)
  noprob <- kernlab::ksvm(
    factor(am) ~ wt,
    data = mtcars,
    kernel = "vanilladot",
    type = "C-svc"
  )
  expect_snapshot(tidypredict_fit(noprob), error = TRUE)
})

ksvm_factor_data <- function(levels, ordered = FALSE, seed = 1) {
  set.seed(seed)
  df <- data.frame(
    x = rnorm(90),
    f = factor(rep(levels, length.out = 90), levels = levels, ordered = ordered)
  )
  df$y <- df$x + as.numeric(df$f) + rnorm(90)
  df
}

ksvm_fit <- function(formula, data, ...) {
  set.seed(1)
  kernlab::ksvm(formula, data = data, kernel = "vanilladot", ...)
}

test_that("unused and ordered factor levels are handled", {
  skip_if_not_installed("kernlab")
  # `ksvm()` builds a full-dummy model matrix itself, so an ordered predictor
  # never reaches `contr.poly`.
  unused <- ksvm_factor_data(c("p", "q", "r"))
  unused$f <- factor(unused$f, levels = c("p", "q", "r", "unused"))
  model <- ksvm_fit(y ~ x + f, unused, type = "eps-svr")
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), unused),
    as.numeric(kernlab::predict(model, unused)),
    tolerance = 1e-10
  )

  ord <- ksvm_factor_data(c("p", "q", "r"), ordered = TRUE)
  model <- ksvm_fit(y ~ x + f, ord, type = "eps-svr")
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), ord),
    as.numeric(kernlab::predict(model, ord)),
    tolerance = 1e-10
  )
})

test_that("a factor level that is not a syntactic name errors (#390)", {
  skip_if_not_installed("kernlab")

  df <- ksvm_factor_data(c("a:b", "c:d", "e"))
  model <- ksvm_fit(y ~ x + f, df, type = "eps-svr")
  expect_snapshot(error = TRUE, tidypredict_fit(model))

  spaced <- ksvm_factor_data(c("a b", "c d", "e"))
  model <- ksvm_fit(y ~ x + f, spaced, type = "eps-svr")
  expect_error(tidypredict_fit(model), "Unable to recover the factor level")

  # Two levels that mangle to the same name, which `ksvm()` then disambiguates
  # with a `.1` suffix.
  clash <- ksvm_factor_data(c("a.b", "a:b", "e"))
  model <- ksvm_fit(y ~ x + f, clash, type = "eps-svr")
  expect_error(tidypredict_fit(model), "Unable to recover the factor level")

  cls <- ksvm_factor_data(c("a:b", "c:d", "e"))
  cls$cls <- factor(ifelse(cls$y > stats::median(cls$y), "a", "b"))
  model <- ksvm_fit(cls ~ x + f, cls, type = "C-svc", prob.model = TRUE)
  expect_error(tidypredict_fit(model), "Unable to recover the factor level")
})

test_that("syntactic factor levels still match predict() (#390)", {
  skip_if_not_installed("kernlab")

  df <- ksvm_factor_data(c("a_b", "cd", "e"))
  model <- ksvm_fit(y ~ x + f, df, type = "eps-svr")
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), df),
    as.numeric(kernlab::predict(model, df)),
    tolerance = 1e-10
  )

  df$cls <- factor(ifelse(df$y > stats::median(df$y), "a", "b"))
  model <- ksvm_fit(cls ~ x + f, df, type = "C-svc", prob.model = TRUE)
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), df),
    unname(kernlab::predict(model, df, type = "probabilities")[, "b"]),
    tolerance = 1e-10
  )
})

ksvm_matrix_fit <- function(cols, ...) {
  set.seed(1)
  x <- matrix(rnorm(30 * length(cols)), ncol = length(cols))
  colnames(x) <- cols
  y <- as.numeric(x %*% seq_along(cols)) + rnorm(30)
  list(
    model = kernlab::ksvm(x, y, kernel = "vanilladot", type = "eps-svr", ...),
    df = as.data.frame(x, check.names = FALSE),
    y = y
  )
}

test_that("matrix columns containing a dot match predict() (#418)", {
  skip_if_not_installed("kernlab")

  # `make.names()` leaves a syntactic name such as `Sepal.Length` alone, so a
  # dot in a matrix column name is not evidence that anything was mangled.
  x <- as.matrix(iris[1:100, c(1, 3, 4)])
  set.seed(1)
  model <- kernlab::ksvm(x, iris[1:100, 2], kernel = "vanilladot")
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), as.data.frame(x)),
    as.numeric(kernlab::predict(model, x)),
    tolerance = 1e-10
  )

  fit <- ksvm_matrix_fit(c("a.b", "a.b.1"))
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(fit$model), fit$df),
    as.numeric(kernlab::predict(fit$model, as.matrix(fit$df))),
    tolerance = 1e-10
  )
})

test_that("syntactic matrix columns match predict() (#418)", {
  skip_if_not_installed("kernlab")

  fit <- ksvm_matrix_fit(c("a_b", "cd"))
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(fit$model), fit$df),
    as.numeric(kernlab::predict(fit$model, as.matrix(fit$df))),
    tolerance = 1e-10
  )
})

test_that("NA in the newdata and in the training data match predict()", {
  skip_if_not_installed("kernlab")

  df <- ksvm_factor_data(c("p", "q", "r"))
  model <- ksvm_fit(y ~ x + f, df, type = "eps-svr")

  nd <- df
  nd$x[1:3] <- NA
  complete <- stats::complete.cases(nd)
  te <- rlang::eval_tidy(tidypredict_fit(model), nd)

  # `predict.ksvm()` drops the incomplete rows rather than returning `NA`.
  expect_true(all(is.na(te[!complete])))
  expect_equal(
    te[complete],
    as.numeric(kernlab::predict(model, nd)),
    tolerance = 1e-10
  )

  trained_with_na <- df
  trained_with_na$x[1:5] <- NA
  model <- ksvm_fit(y ~ x + f, trained_with_na, type = "eps-svr")
  kept <- trained_with_na[stats::complete.cases(trained_with_na), ]

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), kept),
    as.numeric(kernlab::predict(model, kept)),
    tolerance = 1e-10
  )
})

test_that("non-default cost, epsilon and class weights are handled", {
  skip_if_not_installed("kernlab")

  df <- ksvm_factor_data(c("p", "q", "r"))
  model <- ksvm_fit(y ~ x + f, df, type = "eps-svr", C = 10, epsilon = 0.5)
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), df),
    as.numeric(kernlab::predict(model, df)),
    tolerance = 1e-10
  )

  df$cls <- factor(ifelse(df$x + as.numeric(df$f) > 1.5, "a", "b"))
  model <- ksvm_fit(
    cls ~ x + f,
    df,
    type = "C-svc",
    prob.model = TRUE,
    class.weights = c(a = 1, b = 5)
  )
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), df),
    unname(kernlab::predict(model, df, type = "probabilities")[, "b"]),
    tolerance = 1e-10
  )
})

test_that("a coefficient label colliding with a variable name works (#376)", {
  skip_if_not_installed("kernlab")

  set.seed(1)
  df <- data.frame(
    g = factor(rep(c("x1", "y2", "z3"), length.out = 60)),
    gy2 = rnorm(60)
  )
  df$y <- rnorm(60) + as.numeric(df$g) + df$gy2

  model <- kernlab::ksvm(
    y ~ g + gy2,
    data = df,
    kernel = "vanilladot",
    type = "eps-svr"
  )

  expect_false(tidypredict_test(model, df)$alert)
})
