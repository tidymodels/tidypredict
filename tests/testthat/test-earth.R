data("etitanic", package = "earth")

mars_mod <- parsnip::mars(mode = "regression")

test_earth <- function(..., .data = etitanic) {
  p <- invisible(
    tidypredict_test(earth::earth(...), df = .data)
  )
  p$alert
}

test_that("Simple models", {
  expect_false(
    test_earth(age ~ sibsp + parch, data = etitanic)
  )
})

test_that("Less simple models w categorical vars", {
  expect_false(
    test_earth(age ~ ., data = etitanic)
  )
})

test_that("Models w degree work", {
  expect_false(test_earth(age ~ sibsp + parch,
    data = etitanic, degree = 2
  ))
  expect_false(test_earth(age ~ sibsp + parch,
    data = etitanic, degree = 3
  ))
})

test_that("Most pmethods work", {
  pmethods <- c("backward", "none", "exhaustive", "forward", "seqrep")
  res <- lapply(
    pmethods,
    function(x) {
      expect_false(test_earth(age ~ sibsp + parch,
        data = etitanic, pmethod = x
      ))
    }
  )
  expect_false(any(as.logical(res)))
})

test_that("simple binomial works", {
  expect_false(
    test_earth(survived ~ age + sibsp,
      data = etitanic,
      glm = list(family = binomial), .data = etitanic
    )
  )
})

test_that("complex binomial w/ degree works", {
  expect_false(
    test_earth(survived ~ .,
      data = etitanic,
      glm = list(family = binomial), degree = 2, .data = etitanic
    )
  )
})

test_that("Most pmethods work", {
  pmethods <- c("backward", "exhaustive", "forward", "seqrep")
  res <- lapply(
    pmethods,
    function(x) {
      test_earth(survived ~ age + sibsp,
        data = etitanic,
        pmethod = x, glm = list(family = binomial)
      )
    }
  )
  expect_false(any(as.logical(res)))
})


test_that("first degree earth model with different interfaces", {
  f_mod_1 <- earth::earth(Sepal.Length ~ ., data = iris)
  tp_pred_1 <- eval_tidy(tidypredict_fit(f_mod_1), iris)
  earth_pred_1 <- predict(f_mod_1, iris[, -1])[, 1]
  expect_equal(earth_pred_1, tp_pred_1)

  xy_mod_1 <- earth::earth(x = iris[, -1], y = iris$Sepal.Length)
  tp_pred_x1 <- eval_tidy(tidypredict_fit(xy_mod_1), iris[, -1])
  earth_pred_x1 <- predict(xy_mod_1, iris[, -1])[, 1]
  expect_equal(tp_pred_x1, earth_pred_x1)
  expect_equal(tp_pred_x1, tp_pred_1)
})

test_that("2nd degree earth model with different interfaces", {
  f_mod_2 <- earth::earth(Sepal.Length ~ ., data = iris, degree = 2, pmethod = "none")
  tp_pred_2 <- eval_tidy(tidypredict_fit(f_mod_2), iris)
  earth_pred_2 <- predict(f_mod_2, iris[, -1])[, 1]
  expect_equal(earth_pred_2, tp_pred_2)

  xy_mod_2 <- earth::earth(x = iris[, -1], y = iris$Sepal.Length, degree = 2, pmethod = "none")
  tp_pred_x2 <- eval_tidy(tidypredict_fit(xy_mod_2), iris[, -1])
  earth_pred_x2 <- predict(xy_mod_2, iris[, -1])[, 1]
  expect_equal(tp_pred_x2, earth_pred_x2)
  expect_equal(tp_pred_x2, tp_pred_2)
})

test_that("Tests with parsnip returns no alert", {
  # expect_false(
  #   tidypredict_test(
  #     parsnip::fit(
  #       parsnip::set_engine(parsnip::mars(mode = "classification"), "earth"),
  #       survived ~ age + sibsp, data = dplyr::mutate(earth::etitanic, survived = as.factor(survived))
  #     ),
  #     df = etitanic
  #   )$alert
  # )
  expect_false(
    tidypredict_test(
      parsnip::fit(
        parsnip::set_engine(mars_mod, "earth"),
        survived ~ age + sibsp,
        data = etitanic
      ),
      df = etitanic
    )$alert
  )
  expect_false(
    tidypredict_test(
      parsnip::fit(
        parsnip::set_engine(mars_mod, "earth"),
        age ~ sibsp + parch,
        data = etitanic
      ),
      df = etitanic
    )$alert
  )
  expect_false(
    tidypredict_test(
      parsnip::fit(
        parsnip::set_engine(parsnip::mars(prod_degree = 2, mode = "regression"), "earth"),
        age ~ sibsp + parch,
        data = etitanic
      ),
      df = etitanic
    )$alert
  )
  expect_false(
    tidypredict_test(
      parsnip::fit(
        parsnip::set_engine(parsnip::mars(prod_degree = 3, mode = "regression"), "earth"),
        age ~ sibsp + parch,
        data = etitanic
      ),
      df = etitanic
    )$alert
  )
})

test_that("Model can be saved and re-loaded", {
  model <- earth::earth(survived ~ .,
    data = etitanic,
    glm = list(family = binomial), degree = 2
  )
  mp <- tempfile(fileext = ".yml")
  yaml::write_yaml(parse_model(model), mp)
  l <- yaml::read_yaml(mp)
  pm <- as_parsed_model(l)
  expect_snapshot(tidypredict_fit(pm))
})
