# Script to regenerate xgb-v2-regression.rds
# Uses CRAN tidypredict 1.0.1 which produced v1 format (legacy)
# Note: Despite the filename, this is actually version 1 from CRAN

tmp_lib <- tempfile("lib")
dir.create(tmp_lib)
install.packages(
  "tidypredict",
  lib = tmp_lib,
  repos = "https://cloud.r-project.org"
)
library(tidypredict, lib.loc = tmp_lib)

library(xgboost)
set.seed(123)
dtrain <- xgb.DMatrix(
  as.matrix(mtcars[, c("mpg", "cyl")]),
  label = mtcars$hp,
  feature_names = c("mpg", "cyl")
)
model <- xgb.train(
  params = list(
    max_depth = 2,
    objective = "reg:squarederror",
    base_score = 0.5
  ),
  data = dtrain,
  nrounds = 3,
  verbose = 0
)

pm <- parse_model(model)
saveRDS(pm, "tests/testthat/backwards-compat/xgb-v2-regression.rds")
