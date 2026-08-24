# Keep the test suite single-threaded. R CMD check reports a NOTE when the test
# code uses more CPU time than elapsed time, and the toy models used here are far
# too small to benefit from the threads that the engines spin up by default.
#
# Threads are capped at the call site wherever an API allows it (`nthread` for
# xgboost, `num_threads` for lightgbm, `num.threads` for ranger). The rest is
# covered here:
#
# * The reference BLAS is single-threaded, but Debian and Ubuntu default to the
#   pthread build of OpenBLAS, which uses every core and deliberately ignores
#   OMP_NUM_THREADS. earth() runs thousands of tiny least-squares fits, enough to
#   cross OpenBLAS's threading threshold, and measured a CPU-to-elapsed ratio of
#   1.9 on a 4-core Linux runner where a bare dgemm() measured 3.8. Only an
#   in-process call fixes this, since OpenBLAS reads its environment variables
#   when it is loaded, which happens before this file runs.
# * data.table runs its own OpenMP pool inside xgb.model.dt.tree() and
#   lgb.model.dt.tree(), neither of which takes a thread argument, and it also
#   latches its thread count at load time.
# * ranger reads R_RANGER_NUM_THREADS at call time. This is the only way to reach
#   the predict() calls that tidypredict_test() makes internally, and ranger uses
#   std::thread, so the OMP_* variables never applied to it.
# * OMP_* is a backstop for engines that read it before their first parallel
#   region, hence the placement above the library() calls.
Sys.setenv(
  OMP_NUM_THREADS = 1,
  OMP_THREAD_LIMIT = 1,
  R_RANGER_NUM_THREADS = 1
)

if (requireNamespace("RhpcBLASctl", quietly = TRUE)) {
  RhpcBLASctl::blas_set_num_threads(1)
  RhpcBLASctl::omp_set_num_threads(1)
}

if (requireNamespace("data.table", quietly = TRUE)) {
  data.table::setDTthreads(1L)
}

library(testthat)
library(tidypredict)

test_check("tidypredict")
