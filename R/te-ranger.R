te_ranger_fit <- function(parsedmodel) {
  
  str_trees <- parsedmodel[
    substr(parsedmodel$labels, 1, 5) == "tree_",
    ]$labels
  str_trees <- unique(str_trees)
  
  trees <- substr(str_trees, 6, nchar(str_trees))
  trees <- as.integer(trees)
  
  tree_list <- map(
    trees,
    ~ parse_tree(parsedmodel, .x)
  )  
  set_names(tree_list, str_trees)
}

parse_tree <- function(parsedmodel, tree){
  paths <- parsedmodel[parsedmodel$type == "path", ]
  paths <- parsedmodel[parsedmodel$labels == paste0("tree_", tree), ]
  all_paths <- map(
    seq_len(nrow(paths)),
    
    ~ case_formula_ranger(
      paths$estimate[.x],
      paths$field[.x],
      paths$operator[.x],
      paths$split_point[.x]
    )
  )
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


