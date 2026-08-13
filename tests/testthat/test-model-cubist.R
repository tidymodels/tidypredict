test_that("returns the right output", {
  skip_if_not_installed("Cubist")

  model <- Cubist::cubist(
    x = mtcars[, -1],
    y = mtcars$mpg,
    committees = 3
  )
  tf <- tidypredict_fit(model)
  pm <- parse_model(model)

  expect_type(tf, "language")

  expect_s3_class(pm, "list")
  expect_equal(length(pm), 2)
  expect_equal(length(pm$trees), 1)
  expect_equal(pm$general$model, "cubist")
  expect_equal(pm$general$version, 3)

  expect_snapshot(
    rlang::expr_text(tf)
  )
})

test_that("model can be saved and re-loaded", {
  skip_if_not_installed("Cubist")

  model <- Cubist::cubist(
    x = mtcars[, -1],
    y = mtcars$mpg,
    committees = 3
  )
  mp <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(parse_model(model), mp)
  pm <- as_parsed_model(yaml::read_yaml(mp))

  # Against Cubist's own predictions rather than the fitted formula's text:
  # YAML stores fewer digits than a double carries, so the reloaded split
  # thresholds are close but not identical.
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(pm), mtcars),
    unname(predict(model, mtcars)),
    tolerance = 1e-6
  )
})

test_that("formulas produce correct predictions", {
  skip_if_not_installed("Cubist")

  model <- Cubist::cubist(
    x = mtcars[, -1],
    y = mtcars$mpg,
    committees = 3
  )

  # Cubist doesn't work near splits
  # https://github.com/topepo/Cubist/issues/62
  splits <- dplyr::distinct(model$splits, variable, value)
  splits <- map2(splits$variable, splits$value, function(x, y) {
    str2lang(paste("abs(", x, "-", y, ") > 0.0001"))
  })
  non_split_data <- mtcars %>%
    dplyr::filter(!!!splits)

  expect_false(
    tidypredict_test(
      model,
      non_split_data,
      threshold = 0.00001
    )$alert
  )
})

test_that("intercept is done correctly (#58)", {
  skip_if_not_installed("Cubist")

  biomass_tr <- modeldata::biomass %>%
    dplyr::filter(dataset == "Training") %>%
    dplyr::select(-dataset, -sample)

  set.seed(1)
  mod <- Cubist::cubist(
    x = biomass_tr %>% dplyr::select(-HHV),
    y = biomass_tr$HHV
  )

  res <- tidypredict_fit(mod)
  expect_false(any(grepl("list", res)))
})

test_that("doesn't divide by TRUE (#143)", {
  skip_if_not_installed("Cubist")

  rules <- list(quote(37.2 + hp * -0.0318 + wt * -3.88))
  paths <- list(TRUE)

  expect_identical(
    expr_text(make_committee(rules, paths)),
    "37.2 + hp * -0.0318 + wt * -3.88"
  )

  rules <- list(quote(37.2 + hp * -0.0318 + wt * -3.88))
  paths <- list(quote(disp > 95.099998))

  expect_identical(
    expr_text(make_committee(rules, paths)),
    "(37.2 + hp * -0.0318 + wt * -3.88)/(disp > 95.099998)"
  )
})

test_that("predictions match predict()", {
  skip_if_not_installed("Cubist")
  model <- Cubist::cubist(mtcars[, c("wt", "cyl", "disp")], mtcars$mpg)

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), mtcars),
    unname(predict(model, mtcars)),
    tolerance = 1e-6
  )
})

test_that("tidypredict_to_column() works", {
  skip_if_not_installed("Cubist")
  model <- Cubist::cubist(mtcars[, c("wt", "cyl", "disp")], mtcars$mpg)

  res <- tidypredict_to_column(mtcars, model)
  expect_equal(res$fit, unname(predict(model, mtcars)), tolerance = 1e-6)
})

test_that("SQL translation works", {
  skip_if_not_installed("Cubist")
  skip_if_not_installed("dbplyr")
  model <- Cubist::cubist(mtcars[, c("wt", "cyl", "disp")], mtcars$mpg)

  expect_s3_class(tidypredict_sql(model, dbplyr::simulate_dbi()), "sql")
})

test_that("prediction intervals are not supported", {
  skip_if_not_installed("Cubist")
  model <- Cubist::cubist(mtcars[, c("wt", "cyl", "disp")], mtcars$mpg)

  expect_snapshot(error = TRUE, tidypredict_interval(model))
})

test_that("missing predictors match predict() (#294)", {
  skip_if_not_installed("Cubist")
  model <- Cubist::cubist(mtcars[, c("wt", "cyl", "disp")], mtcars$mpg)

  df <- mtcars
  df$wt[1:5] <- NA
  df$disp[4:8] <- NA
  df$cyl[10] <- NA

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), df),
    unname(predict(model, df)),
    tolerance = 1e-6
  )
})

test_that("a row missing every predictor matches predict() (#294)", {
  skip_if_not_installed("Cubist")
  model <- Cubist::cubist(mtcars[, c("wt", "cyl", "disp")], mtcars$mpg)

  df <- mtcars[1, ]
  df[, c("wt", "cyl", "disp")] <- NA_real_

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), df),
    unname(predict(model, df)),
    tolerance = 1e-6
  )
})

test_that("missing values take the stored training mean (#294)", {
  skip_if_not_installed("Cubist")
  model <- Cubist::cubist(mtcars[, c("wt", "cyl", "disp")], mtcars$mpg)

  means <- cubist_attribute_means(model)
  expect_setequal(names(means), c("wt", "cyl", "disp"))

  missing_row <- mtcars[1, ]
  missing_row$wt <- NA_real_
  filled_row <- mtcars[1, ]
  filled_row$wt <- means$wt

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), missing_row),
    rlang::eval_tidy(tidypredict_fit(model), filled_row)
  )
})

cubist_factor_data <- function(n_levels, seed = 1) {
  set.seed(seed)
  n <- 300
  lvls <- letters[seq_len(n_levels)]
  df <- data.frame(
    x = rnorm(n),
    z = rnorm(n),
    f = factor(sample(lvls, n, TRUE), levels = lvls)
  )
  df$y <- 2 * df$x + df$z + 2 * as.integer(df$f) + rnorm(n)
  df
}

test_that("predictions outside a rule's range are clamped (#285)", {
  skip_if_not_installed("Cubist")
  # Cubist holds each rule to the span of the training outcomes it covers,
  # widened at both ends by `extrap` times that span. Without the clamp a
  # linear model runs away on data outside the training range.
  model <- Cubist::cubist(mtcars[, c("wt", "cyl", "disp")], mtcars$mpg)

  far <- mtcars[rep(1, 5), ]
  far$wt <- c(-50, -10, 3, 20, 100)

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), far),
    unname(predict(model, far)),
    tolerance = 1e-6
  )
})

test_that("the clamp applies on ordinary training rows (#285)", {
  skip_if_not_installed("Cubist")
  # The clamp is not only about extrapolation: it engages on rows of the
  # training data itself, so an unclamped formula is wrong on data the model
  # was fitted to.
  df <- cubist_factor_data(6, seed = 5)
  model <- Cubist::cubist(df[, c("x", "z", "f")], df$y)

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), df),
    unname(predict(model, df)),
    tolerance = 1e-6
  )
})

test_that("a rule's widened range does not cross zero (#285)", {
  line <- 'conds="1" cover="9" mean="1" loval="0.5" hival="4" esterr="1"'
  expect_equal(cubist_rule_limits(line, 1), c(0, 7.5))

  line <- 'conds="1" cover="9" mean="-1" loval="-4" hival="-0.5" esterr="1"'
  expect_equal(cubist_rule_limits(line, 1), c(-7.5, 0))

  line <- 'conds="1" cover="9" mean="0" loval="-1" hival="3" esterr="1"'
  expect_equal(cubist_rule_limits(line, 1), c(-5, 7))
})

test_that("a subset condition on a factor matches predict() (#322)", {
  skip_if_not_installed("Cubist")
  # With several levels Cubist writes a rule as `type="3"`, a subset of the
  # levels. The column name is quoted in the model text, so it has to be
  # unquoted before it can name a column.
  df <- cubist_factor_data(6)
  model <- Cubist::cubist(df[, c("x", "z", "f")], df$y)

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), df),
    unname(predict(model, df)),
    tolerance = 1e-6
  )
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(parse_model(model)), df),
    unname(predict(model, df)),
    tolerance = 1e-6
  )
})

test_that("an equality condition on a factor matches predict() (#322)", {
  skip_if_not_installed("Cubist")
  # A rule that names a single level is written as `type="1"`, which
  # `model$splits` leaves out entirely; read from there the rule would apply to
  # every row.
  df <- cubist_factor_data(2)
  model <- Cubist::cubist(df[, c("x", "z", "f")], df$y)

  expect_true(any(grepl('type="1"', strsplit(model$model, "\n")[[1]])))
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), df),
    unname(predict(model, df)),
    tolerance = 1e-6
  )
})

test_that("a factor condition survives more than one committee (#322)", {
  skip_if_not_installed("Cubist")
  df <- cubist_factor_data(4, seed = 3)
  model <- Cubist::cubist(df[, c("x", "z", "f")], df$y, committees = 5)

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), df),
    unname(predict(model, df)),
    tolerance = 1e-6
  )
})

test_that("rows exactly on a split threshold match predict() (#232)", {
  skip_if_not_installed("Cubist")
  # Cubist splits `disp` at 95.1, which is exactly the `disp` of the Lotus
  # Europa. It stores the threshold as the 32-bit float 95.099998 and compares
  # as a float, so that row takes the `<=` branch; comparing in doubles sent it
  # the other way.
  model <- Cubist::cubist(mtcars[, c("wt", "cyl", "disp")], mtcars$mpg)

  on_boundary <- mtcars[mtcars$disp == 95.1, , drop = FALSE]
  expect_equal(nrow(on_boundary), 1)

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), on_boundary),
    unname(predict(model, on_boundary)),
    tolerance = 1e-6
  )
})
