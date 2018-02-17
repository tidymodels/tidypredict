te_ranger_fit <- function(parsedmodel) {
  paths <- parsedmodel %>%
    filter(.data$type == "path") 

  all_paths <- 1:nrow(paths) %>%
    map(~case_formula_ranger(
      ifelse(is.factor(paths$vals[.x]), as.character(paths$vals[.x]), paths$vals[.x]) ,
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
      map(~expr((!! sym(.x$field)) > !! .x$split_point))
  } else {
    right <- NULL
  }

  left <- filter(path, operator == "left")
  if (nrow(left) > 0) {
    left <- left %>%
      split(.$rowid) %>%
      map(~expr((!! sym(.x$field)) <= !! .x$split_point))
  } else {
    left <- NULL
  }


  f <- c(right, left) %>%
    reduce(function(l, r) expr((!! l) & (!! r)))

  expr((!!! f) ~ !! vals)
}

#' @export
parse_model.ranger <- function(model) {
  model_frame <- ranger::treeInfo(model) %>%
    as.tibble() %>%
    mutate(rowid = nodeID) %>%
    rename_all(tolower) 
  
  all_paths <- model_frame %>%
    filter(is.na(.data$leftchild), is.na(.data$rightchild)) %>%
    pull(.data$rowid) %>%
    map(~get_path_ranger(.x, model_frame)) %>%
    bind_rows()
  
  tidy <- model_frame %>%
    as.tibble() %>%
    filter(is.na(.data$leftchild), is.na(.data$rightchild)) %>%
    rowid_to_column("labels") %>%
    mutate(
      labels = paste0("path-", labels),
      type = "path",
      estimate = 0
    ) %>%
    mutate(vals = .data$prediction) %>%
    select(
      .data$labels,
      .data$vals,
      .data$type,
      .data$estimate
    ) %>%
    bind_cols(all_paths) %>%
    add_row(labels = "model", vals = "ranger", type = "variable")
  
  tidy
}


get_path_ranger <- function(row_id, model_frame) {
  
  field <- NULL
  operator <- NULL
  split_point <- NULL
  current_val <- row_id 
  
  for (get_path in row_id:1) {
    current <- model_frame[get_path,]
    if(!is.na(current$leftchild)){
      if(current$leftchild == current_val | current$rightchild == current_val){
        current_val <- current$rowid
        field <- c(
          field,
          as.character(current$splitvarname)
        )
        operator <- c(
          operator,
          paste0(ifelse(current$leftchild == current_val, "left", "right"))
        )
        split_point <- c(
          split_point,
          as.character(current$splitval)
        )
      }
    }
  }
  tibble(
    field = paste0(field, collapse = get_marker()),
    operator = paste0(operator, collapse = get_marker()),
    split_point = paste0(split_point, collapse = get_marker())
  )
}
