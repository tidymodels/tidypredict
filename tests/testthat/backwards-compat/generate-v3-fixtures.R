# Script to regenerate the v3-*.rds fixtures.
#
# Version 3 is the current format, so these are generated with the working tree
# rather than an old release. They exist so that a future change to the v3
# reading path is caught: 1c showed that path can break silently, and #277
# derives a dispatch class at read time that serialized models do not carry.
#
# Each fixture stores the parsed model alongside the predictions the modelling
# package itself produced when the fixture was made. The test therefore compares
# against ground truth rather than against another tidypredict result, and it
# does not need the modelling package installed to run.

devtools::load_all()

save_fixture <- function(name, model, newdata, expected) {
  saveRDS(
    list(
      pm = parse_model(model),
      newdata = newdata,
      expected = expected
    ),
    file.path("tests/testthat/backwards-compat", paste0(name, ".rds"))
  )
}

set.seed(123)

# rpart, a single regression tree
library(rpart)
model <- rpart(mpg ~ wt + cyl, data = mtcars)
save_fixture(
  "v3-rpart-regression",
  model,
  mtcars,
  unname(predict(model, mtcars))
)

# partykit, a single conditional inference tree
library(partykit)
model <- ctree(mpg ~ wt + cyl, data = mtcars)
save_fixture(
  "v3-partykit-regression",
  model,
  mtcars,
  unname(predict(model, mtcars))
)

# randomForest, a forest whose trees are averaged
library(randomForest)
model <- randomForest(mpg ~ wt + cyl, data = mtcars, ntree = 5)
save_fixture(
  "v3-rf-regression",
  model,
  mtcars,
  unname(predict(model, mtcars))
)

# C5.0, a classification tree
library(C50)
iris_x <- iris[, 1:4]
model <- C5.0(iris_x, iris$Species)
save_fixture(
  "v3-c50-classification",
  model,
  iris,
  as.character(predict(model, iris))
)

# xgboost, a gradient boosted ensemble
library(xgboost)
xgb_data <- xgb.DMatrix(
  as.matrix(mtcars[, c("wt", "cyl", "disp")]),
  label = mtcars$mpg
)
model <- xgb.train(
  params = list(max_depth = 2L, objective = "reg:squarederror"),
  data = xgb_data,
  nrounds = 4L
)
save_fixture(
  "v3-xgb-regression",
  model,
  mtcars,
  predict(model, xgb_data)
)
