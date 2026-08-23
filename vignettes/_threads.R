# Keep vignette building single-threaded, for the same reasons as
# tests/testthat.R: R CMD check reports a NOTE when the code uses more CPU time
# than elapsed time, and the toy models here are too small to gain anything from
# extra threads. Sourced from the setup chunk of every vignette that fits a
# model, and it has to run before the engines are loaded, since libgomp and
# OpenBLAS both latch their thread counts when they are loaded.
Sys.setenv(
  OMP_NUM_THREADS = 1,
  OMP_THREAD_LIMIT = 1,
  R_RANGER_NUM_THREADS = 1
)

# Debian and Ubuntu default to the pthread build of OpenBLAS, which uses every
# core and ignores OMP_NUM_THREADS, so it needs an in-process call.
if (requireNamespace("RhpcBLASctl", quietly = TRUE)) {
  RhpcBLASctl::blas_set_num_threads(1)
  RhpcBLASctl::omp_set_num_threads(1)
}

if (requireNamespace("data.table", quietly = TRUE)) {
  data.table::setDTthreads(1L)
}
