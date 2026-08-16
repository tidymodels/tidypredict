# TEMPORARY: diagnostics for #351 calibration on CI. Removed before merge.

test_that("DIAG aorsf calibration moves", {
  skip_if_not_installed("aorsf")

  report <- function(label, model) {
    cal <- aorsf_calibration(model)
    n_trees <- length(model$forest$child_left)
    moves <- list()
    for (t in seq_len(n_trees)) {
      a <- aorsf_tree_info_full(model, t, NULL)
      b <- aorsf_tree_info_full(model, t, cal)
      for (i in seq_along(a$node_splits)) {
        if (is.null(a$node_splits[[i]])) next
        av <- a$node_splits[[i]]$primary$val
        bv <- b$node_splits[[i]]$primary$val
        if (av == bv) next
        rel <- abs(bv - av) / max(1e-8, abs(av))
        moves[[length(moves) + 1L]] <- c(
          tree = t, node = i, dir = sign(bv - av), rel = rel
        )
      }
    }
    if (length(moves) == 0) {
      return(paste0(label, ": no moves"))
    }
    m <- do.call(rbind, moves)
    paste0(
      label, ": ", nrow(m), " moved nodes; up(alg<L) ", sum(m[, "dir"] > 0),
      " down(alg>=R) ", sum(m[, "dir"] < 0),
      "; rel move max ", signif(max(m[, "rel"]), 3),
      " median ", signif(stats::median(m[, "rel"]), 3),
      "; n>1e-12: ", sum(m[, "rel"] > 1e-12)
    )
  }

  set.seed(1)
  m1 <- aorsf::orsf(mtcars, mpg ~ wt + cyl + disp + hp, n_tree = 20)
  set.seed(1)
  m2 <- aorsf::orsf(mtcars, mpg ~ wt + cyl + disp, n_tree = 10)

  # how often does the folded expression tie exactly with the cutpoint?
  ties <- function(model) {
    cal <- aorsf_calibration(model)
    n <- 0
    tot <- 0
    for (t in seq_len(length(model$forest$child_left))) {
      a <- aorsf_tree_info_full(model, t, NULL)
      for (i in seq_along(a$node_splits)) {
        s <- a$node_splits[[i]]$primary
        if (is.null(s)) next
        lc <- build_nested_split_condition(s)[[2]]
        v <- rlang::eval_tidy(lc, cal$x)
        n <- n + sum(v == s$val)
        tot <- tot + length(v)
      }
    }
    paste0("ties ", n, "/", tot)
  }

  testthat::fail(paste(
    paste("aorsf", as.character(utils::packageVersion("aorsf"))),
    report("m1(4 preds,20 trees)", m1),
    report("m2(3 preds,10 trees)", m2),
    ties(m1),
    sep = "\n"
  ))
})
