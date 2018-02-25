te_ranger_fit <- function(parsedmodel) {
  paths <- parsedmodel %>%
    filter(.data$type == "path")

  all_paths <- seq_len(nrow(paths)) %>%
    map(~case_formula_ranger(
      if (is.factor(paths$vals[.x])) as.character(paths$vals[.x]) else paths$vals[.x],
      paths$field[.x],
      paths$operator[.x],
      paths$split_point[.x]
    ))

  expr(case_when(!!! all_paths))
}

case_formula_ranger <- function(vals, field, operator, split_point) {
  marker <- get_marker_regx()

  path <- tibble(
    field = unlist(strsplit(field, marker)),
    operator = unlist(strsplit(operator, marker)),
    split_point = unlist(strsplit(split_point, marker))
  ) %>%
    mutate(split_point = as.numeric(.data$split_point)) %>%
    rowid_to_column()

  right <- filter(path, operator == "right")
  if (nrow(right) > 0) {
    right <- right %>%
      split(.$rowid) %>%
      map(~expr((!! sym(.x$field)) >= !! .x$split_point))
  } else {
    right <- NULL
  }

  left <- filter(path, operator == "left")
  if (nrow(left) > 0) {
    left <- left %>%
      split(.$rowid) %>%
      map(~expr((!! sym(.x$field)) < !! .x$split_point))
  } else {
    left <- NULL
  }

  f <- c(right, left) %>%
    reduce(function(l, r) expr((!! l) & (!! r)))

  expr((!!! f) ~ !! vals)
}
