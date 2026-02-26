# Script to regenerate rf-v2-regression.rds
# Uses CRAN tidypredict 1.0.1 which produced v2 format

tmp_lib <- tempfile("lib")
dir.create(tmp_lib)
install.packages(
  "tidypredict",
  lib = tmp_lib,
  repos = "https://cloud.r-project.org"
)
library(tidypredict, lib.loc = tmp_lib)

library(randomForest)
set.seed(123)
model <- randomForest(mpg ~ cyl + disp, data = mtcars, ntree = 3)

pm <- parse_model(model)
saveRDS(pm, "tests/testthat/backwards-compat/rf-v2-regression.rds")
