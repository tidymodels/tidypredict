context("parse_model")

df <- data.frame(x = c(1, 2, 5, 6, 6), y = c(2, 3, 6, 5, 4))

m <- lm(x ~ y, df)
gm <- glm(x ~ y, df, family = "gaussian")

test_that("Returns a data frame", {
  expect_is(parse_model(m), "data.frame")
  expect_is(parse_model(gm), "data.frame")
})

pm <- parse_model(m)
pgm <- parse_model(gm)

test_that("It has the minium set of columns", {
  expect_equal(colnames(pm)[1], "labels")
  expect_equal(colnames(pm)[3], "type")

  expect_equal(colnames(pgm)[1], "labels")
  expect_equal(colnames(pgm)[3], "type")
})
