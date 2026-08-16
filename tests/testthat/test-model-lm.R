test_that("returns the right output", {
  model <- lm(am ~ wt + cyl, data = mtcars)

  #Don't have stable numbers at the tails across OS
  model$coefficients <- round(model$coefficients, 12)

  tf <- tidypredict_fit(model)
  pm <- parse_model(model)

  expect_type(tf, "language")

  expect_s3_class(pm, "list")
  expect_equal(length(pm), 2)
  expect_equal(pm$general$model, "lm")
  expect_equal(pm$general$version, 2)

  expect_snapshot(
    rlang::expr_text(tf)
  )
})

test_that("model can be saved and re-loaded", {
  model <- lm(am ~ wt + cyl, data = mtcars)

  model$coefficients <- round(model$coefficients, 7)

  pm <- parse_model(model)
  mp <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(pm, mp)
  l <- yaml::read_yaml(mp)
  pm <- as_parsed_model(l)

  expect_identical(
    tidypredict_fit(model),
    tidypredict_fit(pm)
  )
})

test_that("formulas produce correct predictions", {
  mtcars$cyl <- paste0("cyl", mtcars$cyl)

  # normal
  expect_false(
    tidypredict_test(
      lm(mpg ~ wt + am + cyl, data = mtcars),
      mtcars
    )$alert
  )

  # offset
  expect_false(
    tidypredict_test(
      lm(mpg ~ wt, offset = am, data = mtcars),
      mtcars
    )$alert
  )

  # interaction
  expect_false(
    tidypredict_test(
      lm(mpg ~ wt + disp * cyl, data = mtcars),
      mtcars
    )$alert
  )

  # interaction
  expect_false(
    tidypredict_test(
      lm(mpg ~ wt + disp:cyl, data = mtcars),
      mtcars
    )$alert
  )

  # interactions
  expect_false(
    tidypredict_test(
      lm(mpg ~ (wt + disp) * cyl, data = mtcars),
      mtcars
    )$alert
  )
})

test_that("tidypredict works when variable names are subset of other variables", {
  mtcars$cyl <- paste0("cyl", mtcars$cyl)
  mtcars$wt_sq <- mtcars$wt^2
  mtcars$char_cyl <- as.character(mtcars$cyl)
  set.seed(22)
  mtcars$char_cyl_2 <- sample(letters[1:3], size = nrow(mtcars), replace = TRUE)

  model <- lm(
    am ~ wt + wt_sq + char_cyl + char_cyl_2,
    data = mtcars
  )

  expect_false(
    tidypredict_test(
      model,
      mtcars
    )$alert
  )
})

test_that("longest variable name wins with three nested prefixes (#290)", {
  set.seed(1)
  df <- data.frame(
    y = rnorm(30),
    x = rnorm(30),
    xyz = factor(rep(c("A", "B", "C"), each = 10)),
    xy = rnorm(30)
  )

  expect_false(tidypredict_test(lm(y ~ x + xyz + xy, data = df), df)$alert)
  expect_false(tidypredict_test(glm(y ~ x + xyz + xy, data = df), df)$alert)
})

test_that("tidy() works", {
  expect_s3_class(
    tidy(parse_model(lm(mpg ~ ., mtcars))),
    "tbl_df"
  )
})

test_that("rank-deficient fits drop aliased coefficients (#124, #308)", {
  mtcars$vs2 <- mtcars$disp - mtcars$vs

  lm_fit <- lm(mpg ~ ., mtcars)

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(lm_fit), mtcars),
    unname(predict(lm_fit, mtcars))
  )
})

test_that("duplicated predictor columns work (#308)", {
  set.seed(1)
  df <- data.frame(x1 = rnorm(50), x2 = runif(50, 0, 10))
  df$y <- 2 * df$x1 - 0.5 * df$x2 + rnorm(50, sd = 0.3)
  df$xdup <- df$x1

  lm_fit <- lm(y ~ x1 + xdup, data = df)
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(lm_fit), df),
    unname(predict(lm_fit, df))
  )
  expect_equal(
    rlang::eval_tidy(tidypredict_interval(lm_fit), df) +
      unname(predict(lm_fit, df)),
    unname(predict(lm_fit, df, interval = "prediction")[, "upr"])
  )

  glm_fit <- glm(y ~ x1 + xdup, data = df)
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(glm_fit), df),
    unname(predict(glm_fit, df, type = "response"))
  )
})

test_that("zero-variance predictors work (#308)", {
  set.seed(1)
  df <- data.frame(x1 = rnorm(50), x2 = runif(50, 0, 10))
  df$y <- 2 * df$x1 - 0.5 * df$x2 + rnorm(50, sd = 0.3)
  df$yb <- as.integer(df$y > 0)
  df$xconst <- 1

  lm_fit <- lm(y ~ x1 + xconst, data = df)
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(lm_fit), df),
    unname(predict(lm_fit, df))
  )
  expect_equal(
    rlang::eval_tidy(tidypredict_interval(lm_fit), df) +
      unname(predict(lm_fit, df)),
    unname(predict(lm_fit, df, interval = "prediction")[, "upr"])
  )

  glm_fit <- suppressWarnings(
    glm(yb ~ x1 + xconst, data = df, family = binomial())
  )
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(glm_fit), df),
    unname(predict(glm_fit, df, type = "response"))
  )
})

test_that("prediction intervals need a QR decomposition (#308)", {
  pm <- parse_model(lm(mpg ~ wt + cyl, data = mtcars))
  pm$terms <- lapply(pm$terms, function(term) {
    term$qr <- NULL
    term
  })

  expect_snapshot(error = TRUE, tidypredict_interval(pm))
})

test_that("don't add with 0 (#147)", {
  model <- lm(am ~ wt + cyl, data = mtcars)

  model$coefficients <- setNames(c(0, 1.5, 2.2), names(model$coefficients))

  expect_identical(
    tidypredict_fit(model),
    quote((wt * 1.5) + (cyl * 2.2))
  )
})

test_that("gaussian family with identity link works", {
  model <- glm(mpg ~ wt + hp, data = mtcars, family = gaussian())
  fit <- tidypredict_fit(model)
  native <- unname(predict(model, type = "response"))
  tidy <- rlang::eval_tidy(fit, mtcars)
  expect_equal(tidy, native)
})

test_that("binomial family with logit link works", {
  suppressWarnings(
    model <- glm(am ~ wt + hp, data = mtcars, family = binomial())
  )
  fit <- tidypredict_fit(model)
  native <- unname(predict(model, type = "response"))
  tidy <- rlang::eval_tidy(fit, mtcars)
  expect_equal(tidy, native)
})

test_that("logit link keeps probabilities below the double precision floor", {
  suppressWarnings(
    model <- glm(am ~ wt + hp, data = mtcars, family = binomial())
  )
  fit <- tidypredict_fit(model)
  # A linear predictor this far below zero gives a probability that the
  # `1 - 1 / (1 + exp(f))` spelling of the inverse link rounds down to 0.
  newdata <- data.frame(wt = 20, hp = 500)

  expect_equal(
    rlang::eval_tidy(fit, newdata),
    unname(predict(model, newdata, type = "response"))
  )
  expect_gt(rlang::eval_tidy(fit, newdata), 0)
})

test_that("poisson family with log link works", {
  model <- glm(gear ~ wt + hp, data = mtcars, family = poisson())
  fit <- tidypredict_fit(model)
  native <- unname(predict(model, type = "response"))
  tidy <- rlang::eval_tidy(fit, mtcars)
  expect_equal(tidy, native)
})

test_that("Gamma family with inverse link works (#203)", {
  model <- glm(mpg ~ wt + hp, data = mtcars, family = Gamma())

  fit <- tidypredict_fit(model)
  native <- unname(predict(model, type = "response"))
  tidy <- rlang::eval_tidy(fit, mtcars)

  expect_equal(tidy, native)
})

test_that("inverse.gaussian family with 1/mu^2 link works (#204)", {
  model <- glm(mpg ~ wt + hp, data = mtcars, family = inverse.gaussian())

  fit <- tidypredict_fit(model)
  native <- unname(predict(model, type = "response"))
  tidy <- rlang::eval_tidy(fit, mtcars)

  expect_equal(tidy, native)
})

test_that("binomial with probit link works (#205)", {
  suppressWarnings(
    model <- glm(
      am ~ wt + hp,
      data = mtcars,
      family = binomial(link = "probit")
    )
  )

  fit <- tidypredict_fit(model)
  native <- unname(predict(model, type = "response"))
  tidy <- rlang::eval_tidy(fit, mtcars)

  # Uses Bowling et al. approximation to normal CDF for SQL compatibility
  expect_true(all(abs(tidy - native) < 0.001))
})

test_that("binomial with cloglog link works (#206)", {
  suppressWarnings(
    model <- glm(
      am ~ wt + hp,
      data = mtcars,
      family = binomial(link = "cloglog")
    )
  )

  fit <- tidypredict_fit(model)
  native <- unname(predict(model, type = "response"))
  tidy <- rlang::eval_tidy(fit, mtcars)

  expect_equal(tidy, native)
})

test_that("poisson with sqrt link works (#207)", {
  model <- glm(carb ~ wt + hp, data = mtcars, family = poisson(link = "sqrt"))

  fit <- tidypredict_fit(model)
  native <- unname(predict(model, type = "response"))
  tidy <- rlang::eval_tidy(fit, mtcars)

  expect_equal(tidy, native)
})
