context("acceptable")

df <- data.frame(x = c(1, 2, 5, 6, 6), y = c(2, 3, 6, 5, 4))
m <- lm(x ~ as.factor(y), df)

test_that(
  "acceptable_formula fails in expected cases",
  expect_error(acceptable_formula(m))
)
