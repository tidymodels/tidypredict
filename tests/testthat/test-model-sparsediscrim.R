sparsediscrim_probs <- function(model, df) {
  sapply(tidypredict_fit(model), \(f) rlang::eval_tidy(f, df))
}

test_that("returns the right output", {
  skip_if_not_installed("sparsediscrim")

  model <- sparsediscrim::lda_diag(as.matrix(iris[1:4]), iris$Species)

  tf <- tidypredict_fit(model)
  expect_type(tf, "list")
  expect_named(tf, levels(iris$Species))
  expect_true(all(vapply(tf, is.language, logical(1))))

  pm <- parse_model(model)
  expect_s3_class(pm, "list")
  expect_equal(length(pm), 3)
  expect_equal(pm$general$model, "lda_diag")
  expect_equal(pm$general$version, 2)
  expect_equal(pm$classes, levels(iris$Species))

  lps <- lapply(pm$class_terms, build_linear_predictor)
  expect_snapshot(lapply(lps, round_print))
})

test_that("predictions match native predict", {
  skip_if_not_installed("sparsediscrim")

  model <- sparsediscrim::lda_diag(as.matrix(iris[1:4]), iris$Species)

  probs <- sparsediscrim_probs(model, iris)
  native <- as.matrix(predict(model, iris, type = "prob"))

  expect_equal(unname(probs), unname(native))
  expect_equal(unname(rowSums(probs)), rep(1, nrow(iris)))
})

test_that("shrink_mean regularization is handled", {
  skip_if_not_installed("sparsediscrim")

  model <- sparsediscrim::lda_shrink_mean(as.matrix(iris[1:4]), iris$Species)

  expect_equal(parse_model(model)$general$model, "lda_shrink_mean")
  expect_equal(
    unname(sparsediscrim_probs(model, iris)),
    unname(as.matrix(predict(model, iris, type = "prob")))
  )
})

test_that("shrink_cov regularization is handled", {
  skip_if_not_installed("sparsediscrim")

  model <- sparsediscrim::lda_shrink_cov(as.matrix(iris[1:4]), iris$Species)

  expect_equal(parse_model(model)$general$model, "lda_shrink_cov")
  expect_equal(
    unname(sparsediscrim_probs(model, iris)),
    unname(as.matrix(predict(model, iris, type = "prob")))
  )
})

# `predict.lda_emp_bayes_eigen(type = "prob")` builds a rank-deficient
# covariance and returns `NaN` for every row, so the class predictions (an
# argmin over the discriminant scores) are the only usable reference.
test_that("min_distance regularization is handled", {
  skip_if_not_installed("sparsediscrim")

  model <- sparsediscrim::lda_emp_bayes_eigen(
    as.matrix(iris[1:4]),
    iris$Species
  )

  expect_equal(parse_model(model)$general$model, "lda_emp_bayes_eigen")

  probs <- sparsediscrim_probs(model, iris)
  classes <- factor(
    colnames(probs)[apply(probs, 1, which.max)],
    levels = levels(iris$Species)
  )

  expect_equal(unname(rowSums(probs)), rep(1, nrow(iris)))
  expect_equal(classes, predict(model, iris, type = "class"))
})

test_that("the formula interface is handled", {
  skip_if_not_installed("sparsediscrim")

  model <- sparsediscrim::lda_diag(Species ~ ., iris)

  expect_equal(
    unname(sparsediscrim_probs(model, iris)),
    unname(as.matrix(predict(model, iris, type = "prob")))
  )
})

test_that("categorical predictors are handled", {
  skip_if_not_installed("sparsediscrim")

  df <- transform(mtcars, vs = factor(vs), gear = factor(gear))
  model <- sparsediscrim::lda_diag(vs ~ mpg + gear + disp, df)

  expect_equal(
    unname(sparsediscrim_probs(model, df)),
    unname(as.matrix(predict(model, df, type = "prob")))
  )
})

test_that("estimated priors are handled", {
  skip_if_not_installed("sparsediscrim")

  df <- iris[c(1:50, 51:70, 101:110), ]
  df$Species <- droplevels(df$Species)
  model <- sparsediscrim::lda_diag(as.matrix(df[1:4]), df$Species)

  expect_equal(
    unname(sparsediscrim_probs(model, df)),
    unname(as.matrix(predict(model, df, type = "prob")))
  )
})

test_that("binary outcomes are handled", {
  skip_if_not_installed("sparsediscrim")

  df <- transform(mtcars, am = factor(am))
  model <- sparsediscrim::lda_diag(
    as.matrix(mtcars[c("mpg", "wt")]),
    df$am
  )

  tf <- tidypredict_fit(model)
  expect_named(tf, c("0", "1"))

  expect_equal(
    unname(sparsediscrim_probs(model, df)),
    unname(as.matrix(predict(model, df, type = "prob")))
  )
})

test_that("single predictors are handled", {
  skip_if_not_installed("sparsediscrim")

  model <- sparsediscrim::lda_diag(Species ~ Petal.Width, iris)

  expect_equal(
    unname(sparsediscrim_probs(model, iris)),
    unname(as.matrix(predict(model, iris, type = "prob")))
  )
})

test_that("model can be saved and re-loaded", {
  skip_if_not_installed("sparsediscrim")
  skip_if_not_installed("yaml")

  model <- sparsediscrim::lda_diag(as.matrix(iris[1:4]), iris$Species)

  pm <- parse_model(model)
  mp <- tempfile(fileext = ".yml")
  yaml::write_yaml(pm, mp)
  pm <- as_parsed_model(yaml::read_yaml(mp))

  expect_equal(
    sparsediscrim_probs(model, iris),
    sparsediscrim_probs(pm, iris)
  )
})

test_that("inline formula functions are rejected", {
  skip_if_not_installed("sparsediscrim")

  model <- sparsediscrim::lda_diag(Species ~ log(Petal.Width), iris)

  expect_snapshot(error = TRUE, parse_model(model))
})

test_that("tidypredict_test errors for sparsediscrim models", {
  skip_if_not_installed("sparsediscrim")

  model <- sparsediscrim::lda_diag(as.matrix(iris[1:4]), iris$Species)

  expect_snapshot(error = TRUE, tidypredict_test(model, iris))
})

test_that("SQL translation works", {
  skip_if_not_installed("sparsediscrim")
  skip_if_not_installed("dbplyr")

  model <- sparsediscrim::lda_diag(as.matrix(iris[1:4]), iris$Species)

  sql <- tidypredict_sql(model, dbplyr::simulate_dbi())

  expect_named(sql, levels(iris$Species))
  expect_true(all(vapply(sql, \(x) inherits(x, "sql"), logical(1))))
})

test_that("sparsediscrim is handled with parsnip", {
  skip_if_not_installed("sparsediscrim")
  skip_if_not_installed("discrim")

  spec <- parsnip::discrim_linear(engine = "sparsediscrim")
  model <- parsnip::fit(spec, Species ~ ., iris)

  tf <- tidypredict_fit(model)

  expect_type(tf, "list")
  expect_named(tf, levels(iris$Species))

  expect_equal(
    unname(sapply(tf, \(f) rlang::eval_tidy(f, iris))),
    unname(as.matrix(predict(model, iris, type = "prob")))
  )
})

test_that("all regularization methods are handled with parsnip", {
  skip_if_not_installed("sparsediscrim")
  skip_if_not_installed("discrim")

  for (method in c("diagonal", "shrink_cov", "shrink_mean")) {
    spec <- parsnip::discrim_linear(
      engine = "sparsediscrim",
      regularization_method = method
    )
    model <- parsnip::fit(spec, Species ~ ., iris)

    expect_equal(
      unname(sapply(tidypredict_fit(model), \(f) rlang::eval_tidy(f, iris))),
      unname(as.matrix(predict(model, iris, type = "prob")))
    )
  }
})

test_that("categorical predictors are handled with parsnip", {
  skip_if_not_installed("sparsediscrim")
  skip_if_not_installed("discrim")

  df <- transform(mtcars, vs = factor(vs), gear = factor(gear))
  spec <- parsnip::discrim_linear(engine = "sparsediscrim")
  model <- parsnip::fit(spec, vs ~ mpg + gear + disp, df)

  expect_equal(
    unname(sapply(tidypredict_fit(model), \(f) rlang::eval_tidy(f, df))),
    unname(as.matrix(predict(model, df, type = "prob")))
  )
})
