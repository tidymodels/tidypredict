# Script to generate ranger-v2-classification.rds for backwards compatibility testing
#
# This model was created with tidypredict 1.0.1 (CRAN) which used ranger's
# v2 parsed model format with flat path structure in $trees.
#
# Classification models are parsed but error when calling tidypredict_fit().
# This file is used to test that error path.
#
# Run this script to regenerate the test fixture if needed.

# Install CRAN version of tidypredict (1.0.1)
tmp_lib <- tempfile("lib")
dir.create(tmp_lib)
install.packages(
  "tidypredict",
  lib = tmp_lib,
  repos = "https://cloud.r-project.org",
  quiet = TRUE
)

# Load CRAN version
library(tidypredict, lib.loc = tmp_lib)
cat("tidypredict version:", as.character(packageVersion("tidypredict")), "\n")

# ranger is also needed
library(ranger)
cat("ranger version:", as.character(packageVersion("ranger")), "\n")

# Create a classification model
set.seed(123)
model <- ranger::ranger(
  Species ~ Sepal.Length + Sepal.Width,
  data = iris,
  num.trees = 2,
  max.depth = 2,
  seed = 123,
  num.threads = 1
)

pm <- parse_model(model)
cat("\nClassification model:\n")
cat("Version:", pm$general$version, "\n")
cat("Has $trees:", !is.null(pm$trees), "\n")
cat("First prediction:", pm$trees[[1]][[1]]$prediction, "\n")

saveRDS(pm, "ranger-v2-classification.rds")
cat("Saved to ranger-v2-classification.rds\n")

# Cleanup
unlink(tmp_lib, recursive = TRUE)
