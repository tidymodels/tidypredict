context("tidypredict_fit")

df <- mtcars %>%
  mutate(cyl = paste0("cyl", cyl))

m1 <- lm(mpg ~ wt + am, data = df)
m2 <- lm(mpg ~ wt + am + cyl, data = df)
m3 <- lm(mpg ~ wt + + cyl, offset = am, data = df)
m4 <- glm(am ~ wt + cyl, data = df, family = "gaussian")
m5 <- glm(am ~ wt + cyl, data = df, family = "binomial")


p1 <- predict(m1, df)
p2 <- predict(m2, df)
p3 <- predict(m3, df)
p4 <- predict(m4, df)
p5 <- predict(m5, df, type = "response")

f1 <- pull(mutate(df, x = !! tidypredict_fit(m1)))
f2 <- pull(mutate(df, x = !! tidypredict_fit(m2)))
f3 <- pull(mutate(df, x = !! tidypredict_fit(m3)))
f4 <- pull(mutate(df, x = !! tidypredict_fit(m4)))
f5 <- pull(mutate(df, x = !! tidypredict_fit(m5)))

d1 <- p1 - f1
d2 <- p2 - f2
d3 <- p3 - f3
d4 <- p4 - f4
d5 <- p5 - f5


test_that("Individual prediction difference is never above 1e-12", {
  expect_false(any(abs(d1) > 0.000000000001))
  expect_false(any(abs(d2) > 0.000000000001))
  expect_false(any(abs(d3) > 0.000000000001))
  expect_false(any(abs(d4) > 0.000000000001))
  expect_false(any(abs(d5) > 0.000000000001))
})

test_that("Returns a call", {
  expect_is(tidypredict_fit(m1), "call")
  expect_is(tidypredict_fit(m2), "call")
  expect_is(tidypredict_fit(m3), "call")
  expect_is(tidypredict_fit(m4), "call")
  expect_is(tidypredict_fit(m5), "call")
})


mfail <- glm(am ~ wt + cyl, data = df, family = "quasibinomial")
test_that("Fails when model is not guassian or binomial",{
  expect_error(tidypredict_fit(mfail))
})

p <- parse_model(m5)
test_that("Accepts a data frame as the model argument and results are in range",{
  expect_is(tidypredict_fit(p), "call")
  any(abs(p5 - pull(mutate(df, x = !! tidypredict_fit(p)))) > 0.000000000001)
})
