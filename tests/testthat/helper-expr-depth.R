# How deeply the calls in an expression nest. R stops evaluating at a few
# thousand, so this is what tells a left fold apart from a balanced sum.
expr_depth <- function(x) {
  if (!is.call(x)) {
    return(0L)
  }
  args <- as.list(x)[-1]
  if (length(args) == 0) {
    return(1L)
  }
  1L + max(vapply(args, expr_depth, integer(1)))
}
