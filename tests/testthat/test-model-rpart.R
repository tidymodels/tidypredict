test_that("rpart_tree_info returns correct structure", {
  model <- rpart::rpart(mpg ~ cyl + wt, data = mtcars)
  tree_info <- rpart_tree_info(model)

  expect_s3_class(tree_info, "data.frame")
  expect_named(
    tree_info,
    c(
      "nodeID", "leftChild", "rightChild", "splitvarName",
      "splitval", "splitclass", "terminal", "prediction"
    )
  )
})
