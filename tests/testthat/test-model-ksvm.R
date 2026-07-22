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

test_that("Model can be saved and re-loaded", {
  skip_if_not_installed("kernlab")

  set.seed(1)
  model <- kernlab::ksvm(
    mpg ~ wt + hp,
    data = mtcars,
    kernel = "vanilladot",
    type = "eps-svr"
  )

  pm <- parse_model(model)
  mp <- tempfile(fileext = ".yml")
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
