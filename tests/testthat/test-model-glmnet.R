test_that("returns the right output", {
  skip_if_not_installed("glmnet")
  model <- glmnet::glmnet(mtcars[, -1], mtcars$mpg, lambda = 1)

  tf <- tidypredict_fit(model)
  pm <- parse_model(model)

  expect_type(tf, "language")

  expect_s3_class(pm, "list")
  expect_equal(length(pm), 2)
  expect_equal(pm$general$model, "glmnet")
  expect_equal(pm$general$version, 1)

  expect_snapshot(
    round_print(tf)
  )
})

test_that("model can be saved and re-loaded", {
  skip_if_not_installed("yaml")
  skip_if_not_installed("glmnet")
  model <- glmnet::glmnet(mtcars[, -1], mtcars$mpg, lambda = 1)

  pm <- parse_model(model)
  mp <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(pm, mp)
  l <- yaml::read_yaml(mp)
  pm <- as_parsed_model(l)

  expect_identical(
    round_print(tidypredict_fit(model)),
    round_print(tidypredict_fit(pm))
  )
})

test_that("formulas produce correct predictions", {
  skip_if_not_installed("glmnet")
  # gaussian
  expect_false(
    tidypredict_test(
      glmnet::glmnet(mtcars[, -1], mtcars$mpg, family = "gaussian", lambda = 1),
      mtcars[, -1]
    )$alert
  )

  # binomial
  expect_false(
    tidypredict_test(
      glmnet::glmnet(mtcars[, -8], mtcars$vs, family = "binomial", lambda = 1),
      mtcars[, -1]
    )$alert
  )

  # poisson
  expect_false(
    tidypredict_test(
      glmnet::glmnet(mtcars[, -8], mtcars$vs, family = "poisson", lambda = 1),
      mtcars[, -1]
    )$alert
  )
})

test_that("family function syntax works (#197)", {
  skip_if_not_installed("glmnet")
  x <- as.matrix(mtcars[, -1])

  # gaussian()
  model <- glmnet::glmnet(x, mtcars$mpg, family = gaussian(), lambda = 0.5)
  expect_no_error(tidypredict_fit(model))

  # binomial()
  model <- glmnet::glmnet(x, mtcars$am, family = binomial(), lambda = 0.5)
  expect_no_error(tidypredict_fit(model))

  # poisson()
  model <- glmnet::glmnet(x, mtcars$carb, family = poisson(), lambda = 0.5)
  expect_no_error(tidypredict_fit(model))
})

test_that("family string syntax works (#197)", {
  skip_if_not_installed("glmnet")
  x <- as.matrix(mtcars[, -1])

  # "gaussian"
  model <- glmnet::glmnet(x, mtcars$mpg, family = "gaussian", lambda = 0.5)
  expect_no_error(tidypredict_fit(model))

  # "binomial"
  model <- glmnet::glmnet(x, mtcars$am, family = "binomial", lambda = 0.5)
  expect_no_error(tidypredict_fit(model))

  # "poisson"
  model <- glmnet::glmnet(x, mtcars$carb, family = "poisson", lambda = 0.5)
  expect_no_error(tidypredict_fit(model))
})

test_that("fitting options the parser never reads still agree with predict()", {
  skip_if_not_installed("glmnet")
  x <- as.matrix(mtcars[, c("wt", "disp", "hp")])

  models <- list(
    ridge = glmnet::glmnet(x, mtcars$mpg, lambda = 0.5, alpha = 0),
    unstandardized = glmnet::glmnet(
      x,
      mtcars$mpg,
      lambda = 0.5,
      standardize = FALSE
    ),
    no_intercept = glmnet::glmnet(
      x,
      mtcars$mpg,
      lambda = 0.5,
      intercept = FALSE
    ),
    penalty_factor = glmnet::glmnet(
      x,
      mtcars$mpg,
      lambda = 0.5,
      penalty.factor = c(0, 1, 5)
    ),
    limits = glmnet::glmnet(
      x,
      mtcars$mpg,
      lambda = 0.5,
      lower.limits = -1,
      upper.limits = 0.5
    ),
    weights = glmnet::glmnet(
      x,
      mtcars$mpg,
      lambda = 0.5,
      weights = rep(c(1, 3), 16)
    )
  )

  for (model in models) {
    expect_equal(
      rlang::eval_tidy(tidypredict_fit(model), mtcars),
      unname(predict(model, x, type = "response")[, 1])
    )
  }
})

test_that("`NA` in newdata gives the same answer as predict()", {
  skip_if_not_installed("glmnet")
  x <- as.matrix(mtcars[, c("wt", "disp", "hp")])
  na_df <- mtcars
  na_df$wt[c(2, 5)] <- NA
  na_x <- as.matrix(na_df[, c("wt", "disp", "hp")])

  model <- glmnet::glmnet(x, mtcars$mpg, lambda = 0.5)
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), na_df),
    unname(predict(model, na_x, type = "response")[, 1])
  )

  model <- glmnet::glmnet(x, mtcars$am, family = "binomial", lambda = 0.1)
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), na_df),
    unname(predict(model, na_x, type = "response")[, 1])
  )
})

test_that("a factor response works", {
  skip_if_not_installed("glmnet")
  x <- as.matrix(mtcars[, c("wt", "disp", "hp")])
  model <- glmnet::glmnet(
    x,
    factor(mtcars$am),
    family = "binomial",
    lambda = 0.1
  )

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), mtcars),
    unname(predict(model, x, type = "response")[, 1])
  )
})

test_that("a penalty that zeroes every coefficient works", {
  skip_if_not_installed("glmnet")
  x <- as.matrix(mtcars[, c("wt", "disp", "hp")])
  model <- glmnet::glmnet(x, mtcars$mpg, lambda = 1e6)

  # Only the intercept survives, so the formula is a constant
  expect_equal(
    rep(rlang::eval_tidy(tidypredict_fit(model), mtcars), nrow(mtcars)),
    unname(predict(model, x, type = "response")[, 1])
  )
})

test_that("errors if more than 1 penalty is selected", {
  skip_if_not_installed("glmnet")
  model <- glmnet::glmnet(mtcars[, -1], mtcars$mpg)

  expect_snapshot(
    error = TRUE,
    tidypredict_fit(model)
  )

  model <- glmnet::glmnet(mtcars[, -1], mtcars$mpg, lambda = c(1, 5))

  expect_snapshot(
    error = TRUE,
    tidypredict_fit(model)
  )
})

test_that("rejects a model fit with an offset (#296)", {
  skip_if_not_installed("glmnet")

  # glmnet records only a flag, not the offset values, and `predict()` asks for
  # them again as `newoffset`, so there is nothing to rebuild the offset from.
  x <- as.matrix(mtcars[, c("wt", "disp")])

  model <- glmnet::glmnet(
    x,
    mtcars$cyl,
    family = "poisson",
    lambda = 0.05,
    offset = mtcars$am
  )
  expect_snapshot(error = TRUE, tidypredict_fit(model))
  expect_snapshot(error = TRUE, parse_model(model))

  model <- glmnet::glmnet(
    x,
    factor(mtcars$gear),
    family = "multinomial",
    lambda = 0.05,
    offset = matrix(rep(mtcars$am, 3), ncol = 3)
  )
  expect_snapshot(error = TRUE, tidypredict_fit(model))
})

test_that("glmnet are handeld neatly with parsnip", {
  skip_if_not_installed("parsnip")
  skip_if_not_installed("glmnet")
  spec <- parsnip::linear_reg(engine = "glmnet", penalty = 1)

  model <- parsnip::fit(spec, mpg ~ ., mtcars)

  tf <- tidypredict_fit(model)
  pm <- parse_model(model)

  expect_type(tf, "language")

  expect_s3_class(pm, "list")
  expect_equal(length(pm), 2)
  expect_equal(pm$general$model, "glmnet")
  expect_equal(pm$general$version, 1)

  expect_snapshot(
    round_print(tf)
  )
})

test_that("Gamma family works (#200)", {
  skip_if_not_installed("glmnet")
  x <- as.matrix(mtcars[, -1])
  model <- glmnet::glmnet(x, mtcars$mpg, family = Gamma(), lambda = 0.5)

  fit <- tidypredict_fit(model)
  native <- unname(predict(model, x, type = "response")[, 1])
  tidy <- rlang::eval_tidy(fit, mtcars)

  expect_equal(tidy, native)
})

test_that("Cox family works (#201)", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("survival")
  x <- as.matrix(mtcars[, -c(1, 8)])
  y <- survival::Surv(mtcars$mpg, mtcars$vs)
  model <- glmnet::glmnet(
    x,
    y,
    family = "cox",
    lambda = 0.1,
    cox.ties = "breslow"
  )

  fit <- tidypredict_fit(model)
  native <- unname(predict(model, x, type = "link")[, 1])
  tidy <- rlang::eval_tidy(fit, mtcars)

  expect_equal(tidy, native)
})

test_that("multinomial family is supported (#198)", {
  skip_if_not_installed("glmnet")
  model <- glmnet::glmnet(
    as.matrix(iris[, 1:4]),
    iris$Species,
    family = "multinomial",
    lambda = 0.05
  )

  tf <- tidypredict_fit(model)
  pm <- parse_model(model)

  expect_type(tf, "list")
  expect_named(tf, levels(iris$Species))
  expect_true(all(vapply(tf, is.language, logical(1))))

  expect_s3_class(pm, "list")
  expect_equal(length(pm), 3)
  expect_equal(pm$general$model, "glmnet")
  expect_equal(pm$general$family, "multinomial")
  expect_equal(pm$general$version, 1)

  probs <- sapply(tf, function(f) rlang::eval_tidy(f, iris))
  native <- predict(model, as.matrix(iris[, 1:4]), type = "response")[,, 1]

  expect_equal(unname(probs), unname(native))
  expect_equal(unname(rowSums(probs)), rep(1, nrow(iris)))

  lps <- lapply(pm$class_terms, build_linear_predictor)
  expect_snapshot(lapply(lps, round_print))
})

test_that("multinomial model can be saved and re-loaded", {
  skip_if_not_installed("yaml")
  skip_if_not_installed("glmnet")
  model <- glmnet::glmnet(
    as.matrix(iris[, 1:4]),
    iris$Species,
    family = "multinomial",
    lambda = 0.05
  )

  pm <- parse_model(model)
  mp <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(pm, mp)
  pm <- as_parsed_model(yaml::read_yaml(mp))

  from_model <- sapply(tidypredict_fit(model), \(f) rlang::eval_tidy(f, iris))
  from_pm <- sapply(tidypredict_fit(pm), \(f) rlang::eval_tidy(f, iris))

  expect_equal(from_model, from_pm, tolerance = 1e-6)
})

test_that("multinomial handles `NA` in newdata like predict()", {
  skip_if_not_installed("glmnet")
  x <- as.matrix(iris[, 1:4])
  model <- glmnet::glmnet(
    x,
    iris$Species,
    family = "multinomial",
    lambda = 0.05
  )

  na_df <- iris
  na_df$Petal.Length[c(2, 5)] <- NA

  probs <- sapply(tidypredict_fit(model), \(f) rlang::eval_tidy(f, na_df))
  native <- predict(
    model,
    as.matrix(na_df[, 1:4]),
    type = "response"
  )[,, 1]

  expect_equal(unname(probs), unname(native))
})

test_that("multinomial errors with multiple penalties", {
  skip_if_not_installed("glmnet")
  model <- glmnet::glmnet(
    as.matrix(iris[, 1:4]),
    iris$Species,
    family = "multinomial"
  )

  expect_snapshot(error = TRUE, tidypredict_fit(model))
})

test_that("tidypredict_test errors for multinomial models", {
  skip_if_not_installed("glmnet")
  model <- glmnet::glmnet(
    as.matrix(iris[, 1:4]),
    iris$Species,
    family = "multinomial",
    lambda = 0.05
  )

  expect_snapshot(error = TRUE, tidypredict_test(model, iris[, 1:4]))
})

test_that("multinomial is handled with parsnip", {
  skip_if_not_installed("parsnip")
  skip_if_not_installed("glmnet")
  spec <- parsnip::multinom_reg(engine = "glmnet", penalty = 0.05)
  model <- parsnip::fit(spec, Species ~ ., iris)

  tf <- tidypredict_fit(model)

  expect_type(tf, "list")
  expect_named(tf, levels(iris$Species))

  probs <- sapply(tf, function(f) rlang::eval_tidy(f, iris))
  native <- as.matrix(predict(model, iris, type = "prob"))

  expect_equal(unname(probs), unname(native))
})

test_that("multinomial SQL translation works", {
  skip_if_not_installed("dbplyr")
  skip_if_not_installed("glmnet")
  model <- glmnet::glmnet(
    as.matrix(iris[, 1:4]),
    iris$Species,
    family = "multinomial",
    lambda = 0.05
  )

  sql <- tidypredict_sql(model, dbplyr::simulate_dbi())

  expect_named(sql, levels(iris$Species))
  expect_true(all(vapply(sql, \(x) inherits(x, "sql"), logical(1))))
})

test_that("mgaussian family errors with helpful message (#199)", {
  skip_if_not_installed("glmnet")
  x <- as.matrix(mtcars[, -c(1, 4)])
  y <- cbind(mtcars$mpg, mtcars$hp)
  model <- glmnet::glmnet(x, y, family = "mgaussian", lambda = 0.5)

  expect_snapshot(error = TRUE, tidypredict_fit(model))
})

# Tests for .extract_glmnet_multiclass()

test_that(".extract_glmnet_multiclass returns correct structure", {
  skip_if_not_installed("glmnet")
  model <- glmnet::glmnet(
    as.matrix(iris[, 1:4]),
    iris$Species,
    family = "multinomial",
    lambda = 0.5
  )

  result <- .extract_glmnet_multiclass(model)

  expect_type(result, "list")
  expect_length(result, 3)
  expect_named(result, levels(iris$Species))
  expect_type(result[[1]], "character")
})

test_that(".extract_glmnet_multiclass errors on non-multnet model", {
  skip_if_not_installed("glmnet")
  model <- glmnet::glmnet(mtcars[, -1], mtcars$mpg, lambda = 1)

  expect_snapshot(error = TRUE, .extract_glmnet_multiclass(model))
})

test_that(".extract_glmnet_multiclass errors with multiple penalties", {
  skip_if_not_installed("glmnet")
  model <- glmnet::glmnet(
    as.matrix(iris[, 1:4]),
    iris$Species,
    family = "multinomial"
  )

  expect_snapshot(error = TRUE, .extract_glmnet_multiclass(model))
})

test_that(".extract_glmnet_multiclass works with explicit penalty", {
  skip_if_not_installed("glmnet")
  model <- glmnet::glmnet(
    as.matrix(iris[, 1:4]),
    iris$Species,
    family = "multinomial"
  )

  result <- .extract_glmnet_multiclass(model, penalty = 0.01)

  expect_type(result, "list")
  expect_length(result, 3)
})

test_that(".extract_glmnet_multiclass handles sparse coefficients", {
  skip_if_not_installed("glmnet")
  # High penalty should zero out many coefficients

  model <- glmnet::glmnet(
    as.matrix(iris[, 1:4]),
    iris$Species,
    family = "multinomial",
    lambda = 10
  )

  result <- .extract_glmnet_multiclass(model)

  expect_type(result, "list")
  expect_length(result, 3)
})

test_that(".extract_glmnet_multiclass produces correct predictions", {
  skip_if_not_installed("glmnet")
  model <- glmnet::glmnet(
    as.matrix(iris[, 1:4]),
    iris$Species,
    family = "multinomial",
    lambda = 0.01
  )

  eqs <- .extract_glmnet_multiclass(model)
  n_rows <- nrow(iris)

  # Evaluate each linear predictor, recycling scalars to full length
  logits <- sapply(eqs, function(eq) {
    val <- rlang::eval_tidy(rlang::parse_expr(eq), iris)
    if (length(val) == 1) rep(val, n_rows) else val
  })

  # Apply softmax
  exp_logits <- exp(logits)
  probs <- exp_logits / rowSums(exp_logits)

  # Compare to native predictions
  native <- predict(model, as.matrix(iris[, 1:4]), type = "response")[,, 1]

  expect_equal(unname(probs), unname(native), tolerance = 1e-10)
})

# Tests for .build_linear_pred()

test_that(".build_linear_pred handles intercept only", {
  skip_if_not_installed("glmnet")
  result <- .build_linear_pred("(Intercept)", 5.5)

  expect_equal(result, "5.5")
})

test_that(".build_linear_pred handles single predictor", {
  skip_if_not_installed("glmnet")
  result <- .build_linear_pred(c("(Intercept)", "x"), c(1.5, 2.0))

  expect_equal(result, "1.5 + (`x` * 2)")
})

test_that(".build_linear_pred handles multiple predictors", {
  skip_if_not_installed("glmnet")
  result <- .build_linear_pred(
    c("(Intercept)", "x", "y"),
    c(1.0, 2.0, 3.0)
  )

  expect_equal(result, "1 + (`x` * 2) + (`y` * 3)")
})

test_that(".build_linear_pred skips zero coefficients", {
  skip_if_not_installed("glmnet")
  result <- .build_linear_pred(
    c("(Intercept)", "x", "y", "z"),
    c(1.0, 0.0, 2.0, 0.0)
  )

  expect_identical(result, "1 + (`y` * 2)")
})

test_that(".build_linear_pred returns '0' when all coefficients are zero", {
  skip_if_not_installed("glmnet")
  result <- .build_linear_pred(
    c("(Intercept)", "x", "y"),
    c(0, 0, 0)
  )

  expect_equal(result, "0")
})

test_that(".build_linear_pred handles negative coefficients", {
  skip_if_not_installed("glmnet")
  result <- .build_linear_pred(
    c("(Intercept)", "x"),
    c(-1.5, -2.0)
  )

  expect_equal(result, "-1.5 + (`x` * -2)")
})

test_that(".build_linear_pred handles special characters in variable names", {
  skip_if_not_installed("glmnet")
  result <- .build_linear_pred(
    c("(Intercept)", "var with space", "var.with.dots"),
    c(1.0, 2.0, 3.0)
  )

  expect_identical(result, "1 + (`var with space` * 2) + (`var.with.dots` * 3)")
})

test_that(".build_linear_pred handles no intercept", {
  skip_if_not_installed("glmnet")
  result <- .build_linear_pred(c("x", "y"), c(2.0, 3.0))

  expect_equal(result, "(`x` * 2) + (`y` * 3)")
})

test_that(".build_linear_pred handles zero intercept", {
  skip_if_not_installed("glmnet")
  result <- .build_linear_pred(
    c("(Intercept)", "x"),
    c(0, 2.0)
  )

  expect_equal(result, "(`x` * 2)")
})
