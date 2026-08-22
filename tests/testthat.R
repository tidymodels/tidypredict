# Cap OpenMP before any modeling package is loaded. Without this, xgboost,
# lightgbm and friends spin up one thread per core on tiny toy models, which
# inflates the CPU-to-elapsed time ratio enough to trigger a check NOTE.
Sys.setenv(OMP_NUM_THREADS = 1, OMP_THREAD_LIMIT = 1)

library(testthat)
library(tidypredict)

test_check("tidypredict")
