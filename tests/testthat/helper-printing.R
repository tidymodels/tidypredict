round_print <- function(x, digits = 7) {
  x <- expr_text(x)
  x <- gsub("[ \t\r\n]+", " ", x)
  old_values <- regmatches(x, gregexpr("[0-9]+\\.?[0-9]+", x))
  new_values <- lapply(old_values, function(x) signif(as.numeric(x), digits))

  old_values <- unlist(old_values, use.names = FALSE)
  new_values <- unlist(new_values, use.names = FALSE)

  for (i in seq_along(old_values)) {
    x <- sub(old_values[i], new_values[i], x)
  }
  x
}
