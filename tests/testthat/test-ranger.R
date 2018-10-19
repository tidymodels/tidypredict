context("ranger")
set.seed(100)

number_of_trees <- 10
rf_model <- ranger::ranger(Species ~ ., data = iris, num.trees = number_of_trees)

test_that("Returns the correct type", {
  expect_is(parse_model(rf_model), "data.frame")
  expect_is(tidypredict_sql(rf_model, dbplyr::simulate_dbi()), "list")
  expect_is(tidypredict_fit(rf_model), "list")
})

test_that("Correct number of trees are returned", {
  expect_equal(
    length(tidypredict_fit(rf_model)),
    number_of_trees
  )
})

# df_iris <- iris
# df_iris$id <- seq_len(nrow(iris))
# 
# test_that("All tests are under the threshold (5)", {
#   expect_false(
#     tidypredict_test(rf_model, df = df_iris, id_field = id)$alert
#     )
# })