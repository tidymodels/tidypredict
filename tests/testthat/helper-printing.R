round_print <- function(x, digits = 7) {
  x <- expr_text(x)
  x <- gsub("[ \t\r\n]+", " ", x)

  # Match each number in place. Rewriting with `regmatches<-` rather than a
  # loop of `sub()` calls keeps the replacements positional, so a short number
  # cannot match inside a longer one. The pattern also has to cover integers
  # and scientific notation, both of which used to be left unrounded.
  m <- gregexpr("[0-9]+(\\.[0-9]+)?([eE][-+]?[0-9]+)?", x)
  regmatches(x, m) <- lapply(regmatches(x, m), function(v) {
    vapply(
      v,
      function(one) format(signif(as.numeric(one), digits)),
      character(1)
    )
  })

  x
}
