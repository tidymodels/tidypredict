# Formula builder ---------------------------------------------------
te_randomforest_fit <- function(parsedmodel) {
  paths <- parsedmodel %>%
    filter(.data$type == "path")

  all_paths <- seq_len(nrow(paths)) %>%
    map(~case_formula(
      paths$vals[.x],
      paths$field[.x],
      paths$operator[.x],
      paths$split_point[.x]
    ))

  expr(case_when(!!! all_paths))
}

case_formula <- function(vals, field, operator, split_point) {
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

# Model parser ------------------------------------------------------
#' @export
parse_model.randomForest <- function(model) {
  model_frame <- randomForest::getTree(model, labelVar = TRUE) %>%
    as.tibble() %>%
    rowid_to_column()
  
  colnames(model_frame) <- sub(" ", "_", colnames(model_frame))
  
  all_paths <- model_frame %>%
    filter(.data$left_daughter == 0, .data$right_daughter == 0) %>%
    pull(.data$rowid) %>%
    map(~get_path(.x, model_frame)) %>%
    bind_rows()
  
  tidy <- model_frame %>%
    filter(.data$left_daughter == 0, .data$right_daughter == 0) %>%
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
    add_row(labels = "model", vals = "randomForest", type = "variable")
  
  tidy
}

get_path <- function(row_id, model_frame) {
  field <- NULL
  operator <- NULL
  split_point <- NULL
  current_val <- row_id
  
  for (get_path in row_id:1) {
    current <- model_frame[get_path, ]
    if (current$left_daughter != 0) {
      if (current$left_daughter == current_val || current$right_daughter == current_val) {
        field <- c(
          field,
          as.character(current$split_var)
        )
        operator <- c(
          operator,
          if (current$left_daughter == current_val) "left" else "right"
        )
        split_point <- c(
          split_point,
          as.character(current$split_point)
        )
        current_val <- current$rowid
      }
    }
  }
  tibble(
    field = paste0(field, collapse = get_marker()),
    operator = paste0(operator, collapse = get_marker()),
    split_point = paste0(split_point, collapse = get_marker())
  )
}

# Fit method ------------------------------------------------------
#' @export
tidypredict_fit.randomForest <- function(model) {
  parsedmodel <- parse_model(model)
  te_randomforest_fit(parsedmodel)
}
