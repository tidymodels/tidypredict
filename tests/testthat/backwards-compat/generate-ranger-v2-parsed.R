# Script to generate ranger-v2-parsed.rds for backwards compatibility testing
#
# This model was created with tidypredict 1.0.1 (CRAN) which used ranger's
# v2 parsed model format with flat path structure in $trees.
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

# Create a small regression model
set.seed(100)
model <- ranger::ranger(
  mpg ~ cyl + disp,
  data = mtcars,
  num.trees = 2,
  max.depth = 2,
  seed = 100,
  num.threads = 1
)

pm <- parse_model(model)
cat("\nRegression model:\n")
cat("Version:", pm$general$version, "\n")
cat("Has $trees:", !is.null(pm$trees), "\n")
cat("Number of trees:", length(pm$trees), "\n")

saveRDS(pm, "ranger-v2-parsed.rds")
cat("Saved to ranger-v2-parsed.rds\n")

# Cleanup
unlink(tmp_lib, recursive = TRUE)
