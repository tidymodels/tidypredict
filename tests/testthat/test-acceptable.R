df <- data.frame(x = c(1, 2, 5, 6, 6), y = c(2, 3, 6, 5, 4))
m <- lm(x ~ as.factor(y), df)

test_that("acceptable_formula fails in expected cases", {
  expect_error(acceptable_formula(m))
})

test_that("glm family function syntax does not trigger error (#202)", {
  model <- glm(mpg ~ wt + hp, data = mtcars, family = gaussian())
  expect_no_error(acceptable_formula(model))

  model <- glm(am ~ wt + hp, data = mtcars, family = binomial())
  expect_no_error(acceptable_formula(model))

  model <- glm(carb ~ wt + hp, data = mtcars, family = poisson())
  expect_no_error(acceptable_formula(model))
})

test_that("glm family string syntax works (#202)", {
  model <- glm(mpg ~ wt + hp, data = mtcars, family = "gaussian")
  expect_no_error(acceptable_formula(model))

  model <- glm(am ~ wt + hp, data = mtcars, family = "binomial")
  expect_no_error(acceptable_formula(model))

  model <- glm(carb ~ wt + hp, data = mtcars, family = "poisson")
  expect_no_error(acceptable_formula(model))
})
