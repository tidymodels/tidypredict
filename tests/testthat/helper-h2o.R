# Start (or connect to) a local H2O cluster for tests, skipping the calling
# test if h2o/agua are unavailable or the cluster cannot be started.
skip_if_no_h2o <- function() {
  skip_if_not_installed("h2o")
  skip_if_not_installed("agua")
  started <- tryCatch(
    {
      suppressMessages(h2o::h2o.init())
      h2o::h2o.no_progress()
      TRUE
    },
    error = function(e) FALSE
  )
  if (!started) {
    skip("Could not start an H2O cluster.")
  }
}
