test_that("returns the right output", {
  skip_if_not_installed("earth")
  model <- earth::earth(mpg ~ ., data = mtcars)
  model$coefficients <- round(model$coefficients, 12)
  tf <- tidypredict_fit(model)
  pm <- parse_model(model)

  expect_type(tf, "language")

  expect_s3_class(pm, "list")
  expect_equal(length(pm), 2)
  expect_equal(pm$general$model, "earth")
  expect_equal(pm$general$version, 2)

  expect_snapshot(
    rlang::expr_text(tf)
  )
})

test_that("model can be saved and re-loaded", {
  skip_if_not_installed("earth")
  model <- earth::earth(mpg ~ ., data = mtcars)
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
  skip_if_not_installed("earth")
  # Regression - numeric predictors
  expect_false(
    tidypredict_test(
      earth::earth(age ~ sibsp + parch, data = earth::etitanic),
      earth::etitanic
    )$alert
  )

  # Regression - numeric predictors, degree = 2
  expect_false(
    tidypredict_test(
      earth::earth(age ~ sibsp + parch, data = earth::etitanic, degree = 2),
      earth::etitanic
    )$alert
  )

  # Regression - numeric predictors, degree = 3
  expect_false(
    tidypredict_test(
      earth::earth(age ~ sibsp + parch, data = earth::etitanic, degree = 3),
      earth::etitanic
    )$alert
  )

  # Regression - numeric predictors and categorical predictors
  expect_false(
    tidypredict_test(
      earth::earth(age ~ ., data = earth::etitanic),
      earth::etitanic
    )$alert
  )

  # Regression - pmethod = "backwards"
  expect_false(
    tidypredict_test(
      earth::earth(age ~ ., data = earth::etitanic, pmethod = "backward"),
      earth::etitanic
    )$alert
  )

  # Regression - pmethod = "non"
  expect_false(
    tidypredict_test(
      earth::earth(age ~ ., data = earth::etitanic, pmethod = "none"),
      earth::etitanic
    )$alert
  )
  # Regression - pmethod = "exhaustive"
  expect_false(
    tidypredict_test(
      earth::earth(age ~ ., data = earth::etitanic, pmethod = "exhaustive"),
      earth::etitanic
    )$alert
  )

  # Regression - pmethod = "forward"
  expect_false(
    tidypredict_test(
      earth::earth(age ~ ., data = earth::etitanic, pmethod = "forward"),
      earth::etitanic
    )$alert
  )

  # Regression - pmethod = "seqrep"
  expect_false(
    tidypredict_test(
      earth::earth(age ~ ., data = earth::etitanic, pmethod = "seqrep"),
      earth::etitanic
    )$alert
  )

  # binomial
  expect_false(
    tidypredict_test(
      earth::earth(
        survived ~ age + sibsp,
        data = earth::etitanic,
        glm = list(family = binomial)
      ),
      earth::etitanic
    )$alert
  )

  # binomial - w/ degree
  expect_false(
    tidypredict_test(
      earth::earth(
        survived ~ age + sibsp,
        data = earth::etitanic,
        glm = list(family = binomial),
        degree = 2
      ),
      earth::etitanic
    )$alert
  )

  # binomial - pmethod = "backwards"
  expect_false(
    tidypredict_test(
      earth::earth(
        survived ~ .,
        data = earth::etitanic,
        glm = list(family = binomial),
        pmethod = "backward"
      ),
      earth::etitanic
    )$alert
  )

  # binomial - pmethod = "non"
  expect_false(
    tidypredict_test(
      earth::earth(
        survived ~ .,
        data = earth::etitanic,
        glm = list(family = binomial),
        pmethod = "none"
      ),
      earth::etitanic
    )$alert
  )
  # binomial - pmethod = "exhaustive"
  expect_false(
    tidypredict_test(
      earth::earth(
        survived ~ .,
        data = earth::etitanic,
        glm = list(family = binomial),
        pmethod = "exhaustive"
      ),
      earth::etitanic
    )$alert
  )

  # binomial - pmethod = "forward"
  expect_false(
    tidypredict_test(
      earth::earth(
        survived ~ .,
        data = earth::etitanic,
        glm = list(family = binomial),
        pmethod = "forward"
      ),
      earth::etitanic
    )$alert
  )

  # binomial - pmethod = "seqrep"
  expect_false(
    tidypredict_test(
      earth::earth(
        survived ~ .,
        data = earth::etitanic,
        glm = list(family = binomial),
        pmethod = "seqrep"
      ),
      earth::etitanic
    )$alert
  )

  # formula interface
  expect_false(
    tidypredict_test(
      earth::earth(
        Sepal.Length ~ .,
        data = iris
      ),
      iris
    )$alert
  )

  # XY interface
  expect_false(
    tidypredict_test(
      earth::earth(
        x = iris[, -1],
        y = iris$Sepal.Length
      ),
      iris
    )$alert
  )

  # formula interface - degree = 2
  expect_false(
    tidypredict_test(
      earth::earth(
        Sepal.Length ~ .,
        data = iris,
        degree = 2,
        pmethod = "none"
      ),
      iris
    )$alert
  )

  # XY interface - degree = 2
  expect_false(
    tidypredict_test(
      earth::earth(
        x = iris[, -1],
        y = iris$Sepal.Length,
        degree = 2,
        pmethod = "none"
      ),
      iris
    )$alert
  )
})

earth_factor_data <- function(ordered, seed = 1) {
  set.seed(seed)
  n <- 200
  d <- data.frame(
    x = rnorm(n),
    z = rnorm(n),
    f = factor(
      sample(c("a", "b", "c", "d"), n, replace = TRUE),
      ordered = ordered
    )
  )
  d$y <- rnorm(n) + as.numeric(d$f) + 0.5 * d$x
  d
}

test_that("an ordered factor is rejected (#323)", {
  skip_if_not_installed("earth")
  # R fits an ordered factor with `contr.poly`, whose columns are named `f.L`,
  # `f.Q` and `f.C` rather than after the levels. `earth` records no contrasts
  # for `acceptable_lm()` to read, so the level recovered from those names used
  # to reach the formula, where it matched no row.
  d <- earth_factor_data(ordered = TRUE)

  expect_snapshot(
    tidypredict_fit(earth::earth(y ~ x + z + f, data = d)),
    error = TRUE
  )
})

test_that("an unordered factor still matches predict() (#323)", {
  skip_if_not_installed("earth")
  d <- earth_factor_data(ordered = FALSE)
  model <- earth::earth(y ~ x + z + f, data = d)

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), d),
    as.numeric(predict(model, d))
  )
})

test_that("a global non-treatment contrast is rejected (#323)", {
  skip_if_not_installed("earth")
  # `contr.sum` names the columns `f1`, `f2` and `f3`, which are no more levels
  # of `f` than `f.L` is.
  withr::local_options(contrasts = c("contr.sum", "contr.poly"))
  d <- earth_factor_data(ordered = FALSE)

  expect_snapshot(
    tidypredict_fit(earth::earth(y ~ x + z + f, data = d)),
    error = TRUE
  )
})

test_that("an unused factor level still matches predict()", {
  skip_if_not_installed("earth")
  d <- earth_factor_data(ordered = FALSE)
  d$f <- factor(d$f, levels = c(levels(d$f), "unused"))
  model <- earth::earth(y ~ x + z + f, data = d)

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), d),
    as.numeric(predict(model, d))
  )
})

test_that("a factor level colliding with a variable name works", {
  skip_if_not_installed("earth")
  d <- earth_factor_data(ordered = FALSE)
  d$f <- factor(c(a = "x", b = "z", c = "x2", d = "q")[as.character(d$f)])
  model <- earth::earth(y ~ x + z + f, data = d)

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), d),
    as.numeric(predict(model, d))
  )
})

test_that("a factor level containing a colon is rejected", {
  skip_if_not_installed("earth")
  # `earth` names the model matrix column `fb:1`, which `acceptable_lm()` reads
  # as an interaction term and so reports as an unsupported contrast. `predict()`
  # handles the fit fine, so this rejection is a false negative.
  d <- earth_factor_data(ordered = FALSE)
  d$f <- factor(paste0(as.character(d$f), ":1"))

  expect_snapshot(
    tidypredict_fit(earth::earth(y ~ x + z + f, data = d)),
    error = TRUE
  )
})

test_that("`NA` in newdata gives the same answer as predict()", {
  skip_if_not_installed("earth")
  d <- earth_factor_data(ordered = FALSE)
  model <- earth::earth(y ~ x + z + f, data = d)

  na_d <- d
  na_d$x[c(2, 5)] <- NA
  na_d$f[c(1, 3)] <- NA

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), na_d),
    as.numeric(predict(model, na_d))
  )
})

test_that("a knot boundary lands on the same side as predict()", {
  skip_if_not_installed("earth")
  model <- earth::earth(mpg ~ wt, data = mtcars)
  knots <- sort(unique(as.numeric(
    model$cuts[model$selected.terms, , drop = FALSE]
  )))
  expect_gt(length(knots), 1)

  # exactly at each knot, and one ulp either side of it
  nd <- data.frame(wt = c(knots, knots - 1e-12, knots + 1e-12))

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), nd),
    as.numeric(predict(model, nd))
  )
})

test_that("degenerate fit shapes work", {
  skip_if_not_installed("earth")

  # Pure noise prunes back to the intercept, so the formula is a constant
  set.seed(2)
  noise <- data.frame(wt = mtcars$wt, disp = mtcars$disp, mpg = rnorm(32))
  intercept_only <- earth::earth(mpg ~ wt + disp, data = noise)
  expect_length(intercept_only$selected.terms, 1)
  expect_equal(
    rep(rlang::eval_tidy(tidypredict_fit(intercept_only), noise), nrow(noise)),
    as.numeric(predict(intercept_only, noise))
  )

  constant <- mtcars
  constant$mpg <- 5
  model <- earth::earth(mpg ~ wt + disp, data = constant)
  expect_equal(
    rep(rlang::eval_tidy(tidypredict_fit(model), constant), nrow(constant)),
    as.numeric(predict(model, constant))
  )

  single_predictor <- earth::earth(mpg ~ wt, data = mtcars)
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(single_predictor), mtcars),
    as.numeric(predict(single_predictor, mtcars))
  )
})

test_that("probit link works (#194)", {
  skip_if_not_installed("earth")
  model <- earth::earth(
    survived ~ age + sibsp,
    data = earth::etitanic,
    glm = list(family = binomial(link = "probit"))
  )

  fit <- tidypredict_fit(model)
  native <- unname(predict(model, earth::etitanic, type = "response")[, 1])
  tidy <- rlang::eval_tidy(fit, earth::etitanic)

  # Uses Bowling et al. approximation to normal CDF
  expect_equal(tidy, native, tolerance = 0.001)
})

test_that("cloglog link works (#194)", {
  skip_if_not_installed("earth")
  model <- earth::earth(
    survived ~ age + sibsp,
    data = earth::etitanic,
    glm = list(family = binomial(link = "cloglog"))
  )

  fit <- tidypredict_fit(model)
  native <- unname(predict(model, earth::etitanic, type = "response")[, 1])
  tidy <- rlang::eval_tidy(fit, earth::etitanic)

  expect_equal(tidy, native)
})

test_that("Gamma family works (#195)", {
  skip_if_not_installed("earth")
  model <- earth::earth(
    mpg ~ cyl + disp + hp,
    data = mtcars,
    glm = list(family = Gamma)
  )

  fit <- tidypredict_fit(model)
  native <- unname(predict(model, mtcars, type = "response")[, 1])
  tidy <- rlang::eval_tidy(fit, mtcars)

  expect_equal(tidy, native)
})

test_that("inverse.gaussian family works (#195)", {
  skip_if_not_installed("earth")
  model <- earth::earth(
    mpg ~ cyl + disp + hp,
    data = mtcars,
    glm = list(family = inverse.gaussian)
  )

  fit <- tidypredict_fit(model)
  native <- unname(predict(model, mtcars, type = "response")[, 1])
  tidy <- rlang::eval_tidy(fit, mtcars)

  expect_equal(tidy, native)
})

# Tests for .extract_earth_multiclass()

test_that(".extract_earth_multiclass errors on non-earth model", {
  skip_if_not_installed("earth")
  model <- lm(mpg ~ ., data = mtcars)

  expect_snapshot(error = TRUE, .extract_earth_multiclass(model))
})

test_that(".extract_earth_multiclass errors on binary model", {
  skip_if_not_installed("earth")
  suppressWarnings(
    model <- earth::earth(
      vs ~ disp + hp,
      data = mtcars,
      glm = list(family = binomial)
    )
  )

  expect_snapshot(error = TRUE, .extract_earth_multiclass(model))
})

test_that(".extract_earth_multiclass errors on regression model", {
  skip_if_not_installed("earth")
  model <- earth::earth(mpg ~ ., data = mtcars)

  expect_snapshot(error = TRUE, .extract_earth_multiclass(model))
})

test_that(".extract_earth_multiclass returns correct structure", {
  skip_if_not_installed("earth")
  skip_if_not(
    exists("contr.earth.response", where = asNamespace("earth")),
    "earth multiclass not available"
  )
  library(earth)

  suppressWarnings(
    model <- earth(
      Species ~ .,
      data = iris,
      glm = list(family = binomial)
    )
  )

  result <- .extract_earth_multiclass(model)

  expect_type(result, "list")
  expect_length(result, 3)
  expect_named(result, levels(iris$Species))
  expect_type(result[[1]], "character")
})

test_that(".extract_earth_multiclass produces correct predictions", {
  skip_if_not_installed("earth")
  skip_if_not(
    exists("contr.earth.response", where = asNamespace("earth")),
    "earth multiclass not available"
  )
  library(earth)

  suppressWarnings(
    model <- earth(
      Species ~ .,
      data = iris,
      glm = list(family = binomial)
    )
  )

  eqs <- .extract_earth_multiclass(model)
  n_rows <- nrow(iris)

  # Evaluate each expression - earth GLM outputs are already on probability scale
  # (not logits), so we don't apply softmax
  probs <- sapply(eqs, function(eq) {
    val <- rlang::eval_tidy(rlang::parse_expr(eq), iris)
    if (length(val) == 1) rep(val, n_rows) else val
  })

  # Compare to native predictions
  native <- predict(model, iris, type = "response")

  expect_equal(unname(probs), unname(native), tolerance = 1e-6)
})

test_that(".extract_earth_multiclass works with degree > 1", {
  skip_if_not_installed("earth")
  skip_if_not(
    exists("contr.earth.response", where = asNamespace("earth")),
    "earth multiclass not available"
  )
  library(earth)

  suppressWarnings(
    model <- earth(
      Species ~ .,
      data = iris,
      glm = list(family = binomial),
      degree = 2
    )
  )

  result <- .extract_earth_multiclass(model)

  expect_type(result, "list")
  expect_length(result, 3)
  expect_named(result, levels(iris$Species))
})
