# Connect to (or start) a local H2O cluster for tests.
#
# H2O is noisy: `h2o.init()` prints a cluster banner and warns whenever the
# running cluster is older than the newest release, and it reports a genuine
# version mismatch between the cluster and the h2o R package as a warning
# rather than an error. Neither is caught by `suppressMessages()` or
# `tryCatch(error = )`, so they used to surface as testthat warnings instead of
# clean skips. Everything below is muffled, and any condition that means the
# cluster is not usable turns into a skip.
#
# The probe is memoized so that the cluster is started at most once per test
# run, and the cluster is shut down when the suite finishes.

h2o_cluster_probe <- local({
  status <- NULL

  function() {
    if (!is.null(status)) {
      return(status)
    }

    mismatch <- NULL

    status <<- tryCatch(
      withCallingHandlers(
        {
          utils::capture.output(suppressMessages({
            up <- isTRUE(try(h2o::h2o.clusterIsUp(), silent = TRUE))
            if (!up) {
              up <- tryCatch(
                {
                  h2o::h2o.connect()
                  TRUE
                },
                error = function(e) FALSE
              )
            }
            if (!up) {
              h2o::h2o.init()
              withr::defer(
                try(h2o::h2o.shutdown(prompt = FALSE), silent = TRUE),
                testthat::teardown_env()
              )
            }
            h2o::h2o.no_progress()
          }))

          if (!is.null(mismatch)) {
            mismatch
          } else if (
            !identical(
              h2o::h2o.getVersion(),
              as.character(packageVersion("h2o"))
            )
          ) {
            paste0(
              "H2O cluster version (",
              h2o::h2o.getVersion(),
              ") does not match the h2o R package version (",
              packageVersion("h2o"),
              ")."
            )
          } else {
            "ok"
          }
        },
        warning = function(w) {
          msg <- conditionMessage(w)
          if (grepl("version mismatch", msg, ignore.case = TRUE)) {
            mismatch <<- paste0("H2O version mismatch: ", msg)
          }
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) {
        paste0("Could not start an H2O cluster: ", conditionMessage(e))
      }
    )

    status
  }
})

skip_if_no_h2o <- function() {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("h2o")
  testthat::skip_if_not_installed("agua")

  if (!nzchar(Sys.which("java"))) {
    testthat::skip("No Java runtime found; H2O needs a JVM.")
  }

  status <- h2o_cluster_probe()
  if (!identical(status, "ok")) {
    testthat::skip(status)
  }
}
