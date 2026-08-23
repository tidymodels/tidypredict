# Cap OpenMP before any modeling package is loaded. Without this, xgboost,
# lightgbm and friends spin up one thread per core on tiny toy models, which
# inflates the CPU-to-elapsed time ratio enough to trigger a check NOTE.
# OMP_WAIT_POLICY and GOMP_SPINCOUNT stop idle worker threads from busy-waiting,
# which burns CPU time without using any elapsed time.
Sys.setenv(
  OMP_NUM_THREADS = 1,
  OMP_THREAD_LIMIT = 1,
  OMP_WAIT_POLICY = "PASSIVE",
  GOMP_SPINCOUNT = 0,
  # data.table (used by xgb.model.dt.tree() and lgb.model.dt.tree()) and ranger
  # keep their own thread pools and read these at call time.
  R_DATATABLE_NUM_THREADS = 1,
  R_RANGER_NUM_THREADS = 1
)

library(testthat)
library(tidypredict)

test_check("tidypredict")
