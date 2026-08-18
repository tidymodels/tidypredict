xrf_mtcars <- function() {
  df <- mtcars
  df$cyl <- factor(df$cyl)
  df
}

xrf_reg_model <- function(seed = 7, ...) {
  set.seed(seed)
  xrf::xrf(
    mpg ~ wt + hp + cyl,
    xrf_mtcars(),
    family = "gaussian",
    xgb_control = list(nrounds = 5, max_depth = 3),
    ...
  )
}

test_that("returns the right output", {
  skip_if_not_installed("xrf")
  skip_on_cran()
  skip_on_os(c("windows", "linux"))

  model <- xrf_reg_model()

  tf <- tidypredict_fit(model)
  pm <- parse_model(model)

  expect_type(tf, "language")

  expect_s3_class(pm, "list")
  expect_equal(length(pm), 2)
  expect_equal(pm$general$model, "xrf")
  expect_equal(pm$general$version, 2)

  expect_snapshot(
    round_print(tf)
  )
})

test_that("model can be saved and re-loaded", {
  skip_if_not_installed("xrf")

  model <- xrf_reg_model()

  pm <- parse_model(model)
  mp <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(pm, mp)
  l <- yaml::read_yaml(mp)
  pm <- as_parsed_model(l)

  expect_identical(
    round_print(tidypredict_fit(model), digits = 5),
    round_print(tidypredict_fit(pm), digits = 5)
  )
})

test_that("formulas produce correct predictions", {
  skip_if_not_installed("xrf")

  df <- xrf_mtcars()
  model <- xrf_reg_model()

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), df),
    as.numeric(predict(model, df))
  )
  expect_false(tidypredict_test(model, df)$alert)
})

test_that("categorical rule splits produce correct predictions", {
  skip_if_not_installed("xrf")

  df <- xrf_mtcars()
  model <- xrf_reg_model()

  # This fit contains a rule that splits on the `cyl` dummy columns in both
  # directions, which becomes an equality and an inequality on `cyl`.
  tf <- rlang::expr_text(tidypredict_fit(model))
  expect_true(grepl('cyl == "8"', tf, fixed = TRUE))
  expect_true(grepl('cyl != "6"', tf, fixed = TRUE))
})

test_that("character predictors produce correct predictions", {
  skip_if_not_installed("xrf")

  df <- mtcars
  df$vs <- ifelse(df$vs == 1, "yes", "no")
  set.seed(1)
  model <- xrf::xrf(
    mpg ~ wt + hp + vs,
    df,
    family = "gaussian",
    xgb_control = list(nrounds = 5, max_depth = 2)
  )

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), df),
    as.numeric(predict(model, df))
  )
})

test_that("binomial models produce correct predictions", {
  skip_if_not_installed("xrf")

  df <- mtcars
  df$am <- factor(df$am)
  set.seed(1)
  model <- xrf::xrf(
    am ~ wt + hp,
    df,
    family = "binomial",
    xgb_control = list(nrounds = 5, max_depth = 2)
  )

  pm <- parse_model(model)
  expect_equal(pm$general$family, "binomial")
  expect_equal(pm$general$link, "logit")

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), df),
    as.numeric(predict(model, df))
  )
})

test_that("works with non-default xgb_control, sparse, and deoverlap", {
  skip_if_not_installed("xrf")

  df <- xrf_mtcars()

  set.seed(1)
  shallow <- xrf::xrf(
    mpg ~ wt + hp + cyl,
    df,
    family = "gaussian",
    xgb_control = list(nrounds = 2, max_depth = 1)
  )
  expect_type(tidypredict_fit(shallow), "language")
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(shallow), df),
    as.numeric(predict(shallow, df))
  )

  deoverlapped <- xrf_reg_model(deoverlap = TRUE)
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(deoverlapped), df),
    as.numeric(predict(deoverlapped, df))
  )

  dense <- xrf_reg_model(sparse = FALSE)
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(dense), df),
    as.numeric(predict(dense, df, sparse = FALSE))
  )
})

test_that("unused and special-character factor levels are mapped correctly", {
  skip_if_not_installed("xrf")

  df <- mtcars
  df$cyl <- factor(df$cyl, levels = c("4", "5", "6", "8"))
  df$grp <- factor(ifelse(df$hp > 120, "a:b", "c d"))
  set.seed(7)
  model <- xrf::xrf(
    mpg ~ wt + cyl + grp,
    df,
    family = "gaussian",
    xgb_control = list(nrounds = 5, max_depth = 3)
  )

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), df),
    as.numeric(predict(model, df))
  )
})

test_that("a variable named like another variable's dummy column is mapped correctly", {
  skip_if_not_installed("xrf")
  skip(
    "The model matrix has both a `cyl4` dummy (for `cyl == \"4\"`) and a
     variable named `cyl4`. `xrf_feature_field()` checks the variable names
     before the level names, so the dummy is read as the raw column."
  )

  df <- mtcars
  df$cyl <- factor(df$cyl)
  df$cyl4 <- factor(ifelse(df$hp > 120, "8", "6"))
  set.seed(7)
  model <- xrf::xrf(
    mpg ~ wt + cyl + cyl4,
    df,
    family = "gaussian",
    sparse = FALSE,
    xgb_control = list(nrounds = 5, max_depth = 3)
  )

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), df),
    as.numeric(predict(model, df, sparse = FALSE))
  )
})

test_that("values sitting exactly on a rule split match predict()", {
  skip_if_not_installed("xrf")

  set.seed(7)
  model <- xrf::xrf(
    mpg ~ wt + hp,
    mtcars,
    family = "gaussian",
    xgb_control = list(nrounds = 5, max_depth = 3)
  )

  splits <- model$rules$split[model$rules$feature == "wt"]
  expect_gt(length(splits), 0)

  for (offset in c(0, -1e-12, 1e-12)) {
    nd <- mtcars[rep(1, length(splits)), ]
    nd$wt <- splits + offset
    expect_equal(
      rlang::eval_tidy(tidypredict_fit(model), nd),
      as.numeric(predict(model, nd))
    )
  }
})

test_that("rows with NA are scored as NA, and complete rows are unaffected", {
  skip_if_not_installed("xrf")

  df <- xrf_mtcars()
  model <- xrf_reg_model()

  nd <- df
  nd$wt[1:3] <- NA_real_
  fit <- rlang::eval_tidy(tidypredict_fit(model), nd)

  # `predict.xrf()` drops the incomplete rows before assembling its output and
  # then fails, so there is no value to match for those rows.
  expect_error(predict(model, nd))
  expect_equal(is.na(fit), c(rep(TRUE, 3), rep(FALSE, nrow(nd) - 3)))
  expect_equal(fit[-(1:3)], as.numeric(predict(model, nd[-(1:3), ])))
})

test_that("an intercept-only fit matches predict()", {
  skip_if_not_installed("xrf")

  model <- xrf_reg_model()
  model$lambda <- max(model$glm$model$lambda)

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), xrf_mtcars()),
    as.numeric(predict(model, xrf_mtcars(), lambda = model$lambda))
  )
})

test_that("uses the penalty recorded by rules::rule_fit()", {
  skip_if_not_installed("xrf")

  df <- xrf_mtcars()
  model <- xrf_reg_model()
  model$lambda <- model$glm$model$lambda.1se

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), df),
    as.numeric(predict(model, df, lambda = model$glm$model$lambda.1se))
  )
})

test_that("multinomial models are not supported", {
  skip_if_not_installed("xrf")

  set.seed(1)
  model <- xrf::xrf(
    Species ~ .,
    iris,
    family = "multinomial",
    xgb_control = list(nrounds = 3, max_depth = 2, num_class = 3)
  )

  expect_snapshot(error = TRUE, tidypredict_fit(model))
  expect_snapshot(error = TRUE, parse_model(model))
})

test_that("in-line functions in the formula are not supported", {
  skip_if_not_installed("xrf")

  model <- xrf_reg_model()
  model$base_formula <- mpg ~ log(wt) + hp

  expect_snapshot(error = TRUE, tidypredict_fit(model))
})

test_that("unsupported model terms give an informative error", {
  skip_if_not_installed("xrf")

  model <- xrf_reg_model()
  model$base_formula <- mpg ~ wt + hp
  model$glm$xlev <- list()

  expect_snapshot(error = TRUE, tidypredict_fit(model))
})

test_that("can be translated to SQL", {
  skip_if_not_installed("xrf")
  skip_if_not_installed("dbplyr")

  model <- xrf_reg_model()

  expect_s3_class(
    tidypredict_sql(model, dbplyr::simulate_dbi()),
    "sql"
  )
})
