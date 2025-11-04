test_that("returns the right output", {
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

test_that("Model can be saved and re-loaded", {
  model <- earth::earth(mpg ~ ., data = mtcars)
  model$coefficients <- round(model$coefficients, 7)

  pm <- parse_model(model)
  mp <- tempfile(fileext = ".yml")
  yaml::write_yaml(pm, mp)
  l <- yaml::read_yaml(mp)
  pm <- as_parsed_model(l)

  expect_identical(
    tidypredict_fit(model),
    tidypredict_fit(pm)
  )
})

test_that("formulas produces correct predictions", {
  # Regression - numeric predictors
  expect_snapshot(
    tidypredict_test(
      earth::earth(age ~ sibsp + parch, data = earth::etitanic),
      earth::etitanic
    )
  )

  # Regression - numeric predictors, degree = 2
  expect_snapshot(
    tidypredict_test(
      earth::earth(age ~ sibsp + parch, data = earth::etitanic, degree = 2),
      earth::etitanic
    )
  )

  # Regression - numeric predictors, degree = 3
  expect_snapshot(
    tidypredict_test(
      earth::earth(age ~ sibsp + parch, data = earth::etitanic, degree = 3),
      earth::etitanic
    )
  )

  # Regression - numeric predictors and categorical predictors
  expect_snapshot(
    tidypredict_test(
      earth::earth(age ~ ., data = earth::etitanic),
      earth::etitanic
    )
  )

  # Regression - pmethod = "backwards"
  expect_snapshot(
    tidypredict_test(
      earth::earth(age ~ ., data = earth::etitanic, pmethod = "backward"),
      earth::etitanic
    )
  )

  # Regression - pmethod = "non"
  expect_snapshot(
    tidypredict_test(
      earth::earth(age ~ ., data = earth::etitanic, pmethod = "none"),
      earth::etitanic
    )
  )
  # Regression - pmethod = "exhaustive"
  expect_snapshot(
    tidypredict_test(
      earth::earth(age ~ ., data = earth::etitanic, pmethod = "exhaustive"),
      earth::etitanic
    )
  )

  # Regression - pmethod = "forward"
  expect_snapshot(
    tidypredict_test(
      earth::earth(age ~ ., data = earth::etitanic, pmethod = "forward"),
      earth::etitanic
    )
  )

  # Regression - pmethod = "seqrep"
  expect_snapshot(
    tidypredict_test(
      earth::earth(age ~ ., data = earth::etitanic, pmethod = "seqrep"),
      earth::etitanic
    )
  )

  # binomial
  expect_snapshot(
    tidypredict_test(
      earth::earth(
        survived ~ age + sibsp,
        data = earth::etitanic,
        glm = list(family = binomial)
      ),
      earth::etitanic
    )
  )

  # binomial - w/ degree
  expect_snapshot(
    tidypredict_test(
      earth::earth(
        survived ~ age + sibsp,
        data = earth::etitanic,
        glm = list(family = binomial),
        degree = 2
      ),
      earth::etitanic
    )
  )

  # binomial - pmethod = "backwards"
  expect_snapshot(
    tidypredict_test(
      earth::earth(
        survived ~ .,
        data = earth::etitanic,
        glm = list(family = binomial),
        pmethod = "backward"
      ),
      earth::etitanic
    )
  )

  # binomial - pmethod = "non"
  expect_snapshot(
    tidypredict_test(
      earth::earth(
        survived ~ .,
        data = earth::etitanic,
        glm = list(family = binomial),
        pmethod = "none"
      ),
      earth::etitanic
    )
  )
  # binomial - pmethod = "exhaustive"
  expect_snapshot(
    tidypredict_test(
      earth::earth(
        survived ~ .,
        data = earth::etitanic,
        glm = list(family = binomial),
        pmethod = "exhaustive"
      ),
      earth::etitanic
    )
  )

  # binomial - pmethod = "forward"
  expect_snapshot(
    tidypredict_test(
      earth::earth(
        survived ~ .,
        data = earth::etitanic,
        glm = list(family = binomial),
        pmethod = "forward"
      ),
      earth::etitanic
    )
  )

  # binomial - pmethod = "seqrep"
  expect_snapshot(
    tidypredict_test(
      earth::earth(
        survived ~ .,
        data = earth::etitanic,
        glm = list(family = binomial),
        pmethod = "seqrep"
      ),
      earth::etitanic
    )
  )

  # formula interface
  expect_snapshot(
    tidypredict_test(
      earth::earth(
        Sepal.Length ~ .,
        data = iris
      ),
      iris
    )
  )

  # XY interface
  expect_snapshot(
    tidypredict_test(
      earth::earth(
        x = iris[, -1],
        y = iris$Sepal.Length
      ),
      iris
    )
  )

  # formula interface - degree = 2
  expect_snapshot(
    tidypredict_test(
      earth::earth(
        Sepal.Length ~ .,
        data = iris,
        degree = 2,
        pmethod = "none"
      ),
      iris
    )
  )

  # XY interface - degree = 2
  expect_snapshot(
    tidypredict_test(
      earth::earth(
        x = iris[, -1],
        y = iris$Sepal.Length,
        degree = 2,
        pmethod = "none"
      ),
      iris
    )
  )
})
