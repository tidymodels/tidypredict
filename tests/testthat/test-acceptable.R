df <- data.frame(x = c(1, 2, 5, 6, 6), y = c(2, 3, 6, 5, 4))
m <- lm(x ~ as.factor(y), df)

test_that("acceptable_formula fails in expected cases", {
  expect_error(acceptable_formula(m))
})

test_that("every field's contrast is checked (#291)", {
  df <- mtcars
  df$cyl <- factor(df$cyl)
  df$gear <- factor(df$gear)

  model <- lm(
    mpg ~ wt + cyl + gear,
    data = df,
    contrasts = list(cyl = "contr.treatment", gear = "contr.poly")
  )
  expect_snapshot(error = TRUE, acceptable_formula(model))

  model <- lm(
    mpg ~ wt + cyl + gear,
    data = df,
    contrasts = list(cyl = "contr.sum", gear = "contr.poly")
  )
  expect_snapshot(error = TRUE, acceptable_formula(model))

  expect_no_error(acceptable_formula(lm(mpg ~ wt + cyl + gear, data = df)))
})

test_that("glm family function syntax does not trigger error (#202)", {
  model <- glm(mpg ~ wt + hp, data = mtcars, family = gaussian())
  expect_no_error(acceptable_formula(model))

  model <- glm(am ~ wt + hp, data = mtcars, family = binomial())
  expect_no_error(acceptable_formula(model))

  model <- glm(carb ~ wt + hp, data = mtcars, family = poisson())
  expect_no_error(acceptable_formula(model))
})

test_that("acceptable_formula() errors for unsupported objects (#313)", {
  expect_snapshot(error = TRUE, acceptable_formula(NULL))
  expect_snapshot(error = TRUE, acceptable_formula(list()))
})

test_that("glm family string syntax works (#202)", {
  model <- glm(mpg ~ wt + hp, data = mtcars, family = "gaussian")
  expect_no_error(acceptable_formula(model))

  model <- glm(am ~ wt + hp, data = mtcars, family = "binomial")
  expect_no_error(acceptable_formula(model))

  model <- glm(carb ~ wt + hp, data = mtcars, family = "poisson")
  expect_no_error(acceptable_formula(model))
})
