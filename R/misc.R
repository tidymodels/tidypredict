adder <- function(paths) {
  reduce(paths, function(x, y) expr(!!x + !!y))
}
