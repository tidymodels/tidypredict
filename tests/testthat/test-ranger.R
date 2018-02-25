context("ranger")
set.seed(100)
rf_model <- ranger::ranger(Species ~ ., data = iris, num.trees = 100)

test_that("Returns the correct type", {
  expect_is(parse_model(rf_model), "data.frame")
  expect_is(tidypredict_sql(rf_model, dbplyr::simulate_dbi()), "sql")
  expect_is(tidypredict_fit(rf_model), "call")
})

test_that("All tests are under the threshold (5)", {
  expect_false(tidypredict_test(rf_model, threshold = 5, df = iris)$alert)
})

pm <- parse_model(rf_model)
test_pm <- iris %>% tidypredict_to_column(pm)
test_original <- iris %>% tidypredict_to_column(rf_model)

test_that("Parsed model returns same results as model", {
  expect_equal(test_pm, test_original)
})
