test_that("returns the right output", {
  skip_if_not_installed("klaR")

  model <- klaR::NaiveBayes(Species ~ ., data = iris)

  tf <- tidypredict_fit(model)
  expect_type(tf, "list")
  expect_named(tf, levels(iris$Species))
  expect_true(all(vapply(tf, is.language, logical(1))))

  pm <- parse_model(model)
  expect_s3_class(pm, "list")
  expect_equal(length(pm), 3)
  expect_equal(pm$general$model, "NaiveBayes")
  expect_equal(pm$general$version, 2)
  expect_equal(pm$classes, levels(iris$Species))
})

test_that("predictions match native predict", {
  skip_if_not_installed("klaR")

  model <- klaR::NaiveBayes(Species ~ ., data = iris)

  probs <- sapply(tidypredict_fit(model), \(f) rlang::eval_tidy(f, iris))
  native <- predict(model, iris)$posterior

  expect_equal(unname(probs), unname(native))
  expect_equal(unname(rowSums(probs)), rep(1, nrow(iris)))
})

test_that("categorical and logical predictors are handled", {
  skip_if_not_installed("klaR")

  df <- transform(
    mtcars,
    cyl = factor(cyl),
    gear = factor(gear),
    vs = vs == 1
  )
  model <- klaR::NaiveBayes(cyl ~ mpg + gear + vs, data = df)

  probs <- sapply(tidypredict_fit(model), \(f) rlang::eval_tidy(f, df))

  expect_equal(unname(probs), unname(predict(model, df)$posterior))
})

test_that("zero-probability levels use the predict threshold", {
  skip_if_not_installed("klaR")

  df <- transform(mtcars, cyl = factor(cyl), gear = factor(gear))
  model <- klaR::NaiveBayes(cyl ~ gear, data = df)

  # `gear == 5` never happens for `cyl == 8`
  expect_true(any(model$tables$gear == 0))

  probs <- sapply(tidypredict_fit(model), \(f) rlang::eval_tidy(f, df))

  expect_equal(unname(probs), unname(predict(model, df)$posterior))
})

test_that("binary outcomes are handled", {
  skip_if_not_installed("klaR")

  df <- transform(mtcars, am = factor(am))
  model <- klaR::NaiveBayes(am ~ mpg + wt, data = df)

  tf <- tidypredict_fit(model)
  expect_named(tf, c("0", "1"))

  probs <- sapply(tf, \(f) rlang::eval_tidy(f, df))

  expect_equal(unname(probs), unname(predict(model, df)$posterior))
})

test_that("prior, fL, and the x/grouping interface are handled", {
  skip_if_not_installed("klaR")

  df <- transform(mtcars, cyl = factor(cyl), gear = factor(gear))

  for (prior in list(NULL, c(0.2, 0.3, 0.5))) {
    for (fL in c(0, 1)) {
      model <- klaR::NaiveBayes(
        df[c("mpg", "gear")],
        df$cyl,
        prior = prior,
        fL = fL
      )

      probs <- sapply(tidypredict_fit(model), \(f) rlang::eval_tidy(f, df))

      expect_equal(
        unname(probs),
        unname(predict(model, df[c("mpg", "gear")])$posterior)
      )
    }
  }
})

test_that("a single predictor is handled", {
  skip_if_not_installed("klaR")

  model <- klaR::NaiveBayes(Species ~ Petal.Width, data = iris)

  expect_snapshot(round_print(tidypredict_fit(model)[["setosa"]]))

  probs <- sapply(tidypredict_fit(model), \(f) rlang::eval_tidy(f, iris))

  expect_equal(unname(probs), unname(predict(model, iris)$posterior))
})

test_that("model can be saved and re-loaded", {
  skip_if_not_installed("klaR")
  skip_if_not_installed("yaml")

  model <- klaR::NaiveBayes(Species ~ ., data = iris)

  pm <- parse_model(model)
  mp <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(pm, mp)
  pm <- as_parsed_model(yaml::read_yaml(mp))

  from_model <- sapply(tidypredict_fit(model), \(f) rlang::eval_tidy(f, iris))
  from_pm <- sapply(tidypredict_fit(pm), \(f) rlang::eval_tidy(f, iris))

  expect_equal(from_model, from_pm, tolerance = 1e-6)
})

test_that("kernel density fits are rejected", {
  skip_if_not_installed("klaR")

  model <- klaR::NaiveBayes(Species ~ ., data = iris, usekernel = TRUE)

  expect_snapshot(error = TRUE, tidypredict_fit(model))
  expect_snapshot(error = TRUE, parse_model(model))
})

test_that("tidypredict_test errors for NaiveBayes models", {
  skip_if_not_installed("klaR")

  model <- klaR::NaiveBayes(Species ~ ., data = iris)

  expect_snapshot(error = TRUE, tidypredict_test(model, iris))
})

test_that("SQL translation works", {
  skip_if_not_installed("klaR")
  skip_if_not_installed("dbplyr")

  model <- klaR::NaiveBayes(Species ~ ., data = iris)

  sql <- tidypredict_sql(model, dbplyr::simulate_dbi())

  expect_named(sql, levels(iris$Species))
  expect_true(all(vapply(sql, \(x) inherits(x, "sql"), logical(1))))
})

test_that("naive_bayes returns the right output", {
  skip_if_not_installed("naivebayes")

  model <- naivebayes::naive_bayes(Species ~ ., data = iris)

  tf <- tidypredict_fit(model)
  expect_type(tf, "list")
  expect_named(tf, levels(iris$Species))
  expect_true(all(vapply(tf, is.language, logical(1))))

  pm <- parse_model(model)
  expect_s3_class(pm, "list")
  expect_equal(length(pm), 3)
  expect_equal(pm$general$model, "naive_bayes")
  expect_equal(pm$general$version, 2)
  expect_equal(pm$classes, levels(iris$Species))
})

test_that("naive_bayes predictions match native predict", {
  skip_if_not_installed("naivebayes")

  model <- naivebayes::naive_bayes(Species ~ ., data = iris)

  probs <- sapply(tidypredict_fit(model), \(f) rlang::eval_tidy(f, iris))
  native <- predict(model, iris[names(model$tables)], type = "prob")

  expect_equal(unname(probs), unname(native))
  expect_equal(unname(rowSums(probs)), rep(1, nrow(iris)))
})

test_that("naive_bayes handles categorical, logical, and Poisson predictors", {
  skip_if_not_installed("naivebayes")

  df <- transform(
    mtcars,
    cyl = factor(cyl),
    gear = factor(gear),
    vs = vs == 1,
    carb = as.integer(carb)
  )

  for (usepoisson in c(FALSE, TRUE)) {
    model <- suppressWarnings(naivebayes::naive_bayes(
      cyl ~ mpg + gear + vs + carb,
      data = df,
      usepoisson = usepoisson
    ))

    probs <- sapply(tidypredict_fit(model), \(f) rlang::eval_tidy(f, df))

    expect_equal(
      unname(probs),
      unname(predict(model, df[names(model$tables)], type = "prob"))
    )
  }
})

test_that("naive_bayes zero-probability levels use the predict threshold", {
  skip_if_not_installed("naivebayes")

  df <- transform(mtcars, cyl = factor(cyl), gear = factor(gear))
  model <- suppressWarnings(naivebayes::naive_bayes(cyl ~ gear, data = df))

  # `gear == 5` never happens for `cyl == 8`
  expect_true(any(model$tables$gear == 0))

  probs <- sapply(tidypredict_fit(model), \(f) rlang::eval_tidy(f, df))

  expect_equal(
    unname(probs),
    unname(predict(model, df[names(model$tables)], type = "prob"))
  )
})

test_that("naive_bayes handles laplace and prior", {
  skip_if_not_installed("naivebayes")

  df <- transform(mtcars, cyl = factor(cyl), gear = factor(gear))

  for (prior in list(NULL, c(0.2, 0.3, 0.5))) {
    for (laplace in c(0, 1)) {
      model <- suppressWarnings(naivebayes::naive_bayes(
        df[c("mpg", "gear")],
        df$cyl,
        prior = prior,
        laplace = laplace
      ))

      probs <- sapply(tidypredict_fit(model), \(f) rlang::eval_tidy(f, df))

      expect_equal(
        unname(probs),
        unname(predict(model, df[c("mpg", "gear")], type = "prob"))
      )
    }
  }
})

test_that("naive_bayes handles binary outcomes and a single predictor", {
  skip_if_not_installed("naivebayes")

  df <- transform(mtcars, am = factor(am))
  model <- naivebayes::naive_bayes(am ~ mpg, data = df)

  tf <- tidypredict_fit(model)
  expect_named(tf, c("0", "1"))
  expect_snapshot(round_print(tf[["0"]]))

  probs <- sapply(tf, \(f) rlang::eval_tidy(f, df))

  expect_equal(
    unname(probs),
    unname(predict(model, df[names(model$tables)], type = "prob"))
  )
})

test_that("naive_bayes model can be saved and re-loaded", {
  skip_if_not_installed("naivebayes")
  skip_if_not_installed("yaml")

  model <- naivebayes::naive_bayes(Species ~ ., data = iris)

  pm <- parse_model(model)
  mp <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(pm, mp)
  pm <- as_parsed_model(yaml::read_yaml(mp))

  from_model <- sapply(tidypredict_fit(model), \(f) rlang::eval_tidy(f, iris))
  from_pm <- sapply(tidypredict_fit(pm), \(f) rlang::eval_tidy(f, iris))

  expect_equal(from_model, from_pm, tolerance = 1e-6)
})

test_that("naive_bayes kernel density fits are rejected", {
  skip_if_not_installed("naivebayes")

  model <- naivebayes::naive_bayes(Species ~ ., data = iris, usekernel = TRUE)

  expect_snapshot(error = TRUE, tidypredict_fit(model))
  expect_snapshot(error = TRUE, parse_model(model))
})

test_that("tidypredict_test errors for naive_bayes models", {
  skip_if_not_installed("naivebayes")

  model <- naivebayes::naive_bayes(Species ~ ., data = iris)

  expect_snapshot(error = TRUE, tidypredict_test(model, iris))
})

test_that("naive_bayes SQL translation works", {
  skip_if_not_installed("naivebayes")
  skip_if_not_installed("dbplyr")

  model <- naivebayes::naive_bayes(Species ~ ., data = iris)

  sql <- tidypredict_sql(model, dbplyr::simulate_dbi())

  expect_named(sql, levels(iris$Species))
  expect_true(all(vapply(sql, \(x) inherits(x, "sql"), logical(1))))
})

test_that("naive_bayes is handled with parsnip", {
  skip_if_not_installed("naivebayes")
  skip_if_not_installed("discrim")

  spec <- parsnip::set_engine(
    parsnip::naive_Bayes(),
    "naivebayes",
    usekernel = FALSE
  )
  model <- parsnip::fit(spec, Species ~ ., iris)

  tf <- tidypredict_fit(model)

  expect_type(tf, "list")
  expect_named(tf, levels(iris$Species))

  probs <- sapply(tf, \(f) rlang::eval_tidy(f, iris))
  native <- as.matrix(predict(model, iris, type = "prob"))

  expect_equal(unname(probs), unname(native))
})

test_that("NaiveBayes is handled with parsnip", {
  skip_if_not_installed("klaR")
  skip_if_not_installed("discrim")

  spec <- parsnip::set_engine(
    parsnip::naive_Bayes(),
    "klaR",
    usekernel = FALSE
  )
  model <- parsnip::fit(spec, Species ~ ., iris)

  tf <- tidypredict_fit(model)

  expect_type(tf, "list")
  expect_named(tf, levels(iris$Species))

  probs <- sapply(tf, \(f) rlang::eval_tidy(f, iris))
  native <- as.matrix(predict(model, iris, type = "prob"))

  expect_equal(unname(probs), unname(native))
})
