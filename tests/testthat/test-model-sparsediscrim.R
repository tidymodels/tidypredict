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

test_that("a row far outside the training range is a documented divergence (#299)", {
  skip_if_not_installed("sparsediscrim")
  # `sparsediscrim` normalizes its class scores with the textbook softmax, so
  # it returns `NaN` for a row whose scores are large enough that `exp()`
  # overflows. `tidypredict` subtracts the class score inside each `exp()` and
  # returns the distribution those scores imply, which is the one every other
  # backend's `predict()` gives for such a row.
  model <- sparsediscrim::lda_diag(as.matrix(iris[1:4]), iris$Species)

  far <- iris[rep(1, 2), ]
  far[1:4] <- list(c(100, 1e3), c(100, -1e3), c(100, 1e3), c(100, -1e3))

  expect_true(anyNA(as.matrix(predict(model, far, type = "prob"))))

  probs <- sparsediscrim_probs(model, far)
  expect_false(anyNA(probs))
  expect_equal(unname(rowSums(probs)), rep(1, nrow(far)))

  # Rows the model can actually have seen are unaffected
  expect_equal(
    unname(sparsediscrim_probs(model, iris)),
    unname(as.matrix(predict(model, iris, type = "prob")))
  )
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

sparsediscrim_factor_data <- function(levels, ordered = FALSE, seed = 1) {
  set.seed(seed)
  df <- data.frame(
    x = rnorm(90),
    f = factor(rep(levels, length.out = 90), levels = levels, ordered = ordered)
  )
  df$cls <- factor(ifelse(df$x + as.numeric(df$f) > 1.5, "a", "b"))
  df
}

test_that("special-character, unused and ordered factor levels are handled", {
  skip_if_not_installed("sparsediscrim")
  # The formula method builds its own full-dummy model matrix and names every
  # column after the level it stands for, whatever the contrasts say.
  colon <- sparsediscrim_factor_data(c("a:b", "c:d", "e"))
  model <- sparsediscrim::lda_diag(cls ~ x + f, colon)
  expect_equal(
    unname(sparsediscrim_probs(model, colon)),
    unname(as.matrix(predict(model, colon, type = "prob")))
  )

  unused <- sparsediscrim_factor_data(c("p", "q", "r"))
  unused$f <- factor(unused$f, levels = c("p", "q", "r", "unused"))
  model <- suppressWarnings(sparsediscrim::lda_diag(cls ~ x + f, unused))
  expect_equal(
    unname(sparsediscrim_probs(model, unused)),
    unname(as.matrix(predict(model, unused, type = "prob")))
  )

  ord <- sparsediscrim_factor_data(c("p", "q", "r"), ordered = TRUE)
  model <- sparsediscrim::lda_diag(cls ~ x + f, ord)
  expect_equal(
    unname(sparsediscrim_probs(model, ord)),
    unname(as.matrix(predict(model, ord, type = "prob")))
  )
})

test_that("newdata containing NA matches predict()", {
  skip_if_not_installed("sparsediscrim")

  df <- sparsediscrim_factor_data(c("p", "q", "r"))
  model <- sparsediscrim::lda_diag(cls ~ x + f, df)

  nd <- df
  nd$x[1:3] <- NA

  probs <- sparsediscrim_probs(model, nd)
  native <- as.matrix(predict(model, nd, type = "prob"))

  expect_true(anyNA(native))
  expect_equal(unname(probs), unname(native))
})

test_that("a colliding model matrix column is rejected (#398)", {
  skip_if_not_installed("sparsediscrim")

  set.seed(1)
  df <- data.frame(
    g = factor(rep(c("x1", "y2", "z3"), length.out = 60)),
    gy2 = rnorm(60)
  )
  df$cls <- factor(ifelse(df$gy2 + as.numeric(df$g) > 2, "a", "b"))

  model <- sparsediscrim::lda_diag(cls ~ g + gy2, df)

  # `predict()` selects the duplicated `gy2` column twice and never sees the
  # predictor, so it disagrees with the fit it came from.
  expect_snapshot(tidypredict_fit(model), error = TRUE)
})

test_that("an ordered factor is rejected with parsnip (#393)", {
  skip_if_not_installed("sparsediscrim")
  skip_if_not_installed("discrim")

  df <- sparsediscrim_factor_data(c("p", "q", "r"), ordered = TRUE)
  spec <- parsnip::discrim_linear(engine = "sparsediscrim")
  model <- parsnip::fit(spec, cls ~ x + f, df)

  expect_snapshot(tidypredict_fit(model), error = TRUE)
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
  mp <- withr::local_tempfile(fileext = ".yml")
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
