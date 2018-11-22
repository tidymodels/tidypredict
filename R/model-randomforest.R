# Formula builder ---------------------------------------------------
te_randomforest_fit <- function(parsedmodel){
  str_trees <- parsedmodel[
    substr(parsedmodel$labels, 1, 5) == "tree_",
    ]$labels
  str_trees <- unique(str_trees)
  
  trees <- substr(str_trees, 6, nchar(str_trees))
  trees <- as.integer(trees)
  
  tree_list <- map(
    trees,
    ~ parsed_rf_tree(parsedmodel, .x)
  )  
  set_names(tree_list, str_trees)
}


parsed_rf_tree <- function(parsedmodel, tree) {
  paths <- parsedmodel[parsedmodel$labels == paste0("tree_", tree), ]
  
  all_paths <- seq_len(nrow(paths)) %>%
    map(~case_rf_formula(
      paths$vals[.x],
      paths$field[.x],
      paths$operator[.x],
      paths$split_point[.x]
    ))
  
  expr(case_when(!!! all_paths))
}

case_rf_formula <- function(vals, field, operator, split_point) {
  marker <- get_marker_regx()
  
  path <- tibble(
    field = unlist(strsplit(field, marker)),
    operator = unlist(strsplit(operator, marker)),
    split_point = unlist(strsplit(split_point, marker))
  ) 
  path$split_point <- as.numeric(path$split_point)
  path$rowid <- seq_len(nrow(path))
  
  right <- path[path$operator == "right", ]
  
  if (nrow(right) > 0) {
    right <- split(right, right$rowid)
    right <- map(right, ~expr((!! sym(.x$field)) > !! .x$split_point))
  } else {
    right <- NULL
  }
  
  left <- path[path$operator == "left", ]
  
  if (nrow(left) > 0) {
    left <- split(left, left$rowid)
    left <- map(left, ~expr((!! sym(.x$field)) <= !! .x$split_point))
  } else {
    left <- NULL
  }
  
  rl <- c(right, left) 
  
  f <- reduce(rl, function(l, r) expr((!! l) & (!! r)))
  
  expr((!! f) ~ !! vals)
}


# Model parser ------------------------------------------------------
#' @export
parse_model.randomForest <- function(model) {
  res <- map_df(
    seq_len(model$ntree),
    ~ get_rf_tree(model, .x)
  )
  res[, c(6, 1, 2, 3, 4, 5)]
}


get_rf_tree <- function(model, no_tree){
  
  tree <- randomForest::getTree(
    model, 
    no_tree, 
    labelVar = TRUE
  )
  
  tree$rowid <- seq_len(nrow(tree))
  colnames(tree) <- sub(" ", "_", colnames(tree))
  all_paths <- tree[tree$left_daughter == 0 & tree$right_daughter == 0, ]
  all_paths <- all_paths$rowid
  all_paths <- map_df(all_paths, ~ get_rf_path(.x, tree))
  all_paths <- as.data.frame(all_paths)
  all_paths$labels <- paste0("tree_", no_tree)
  all_paths
}

get_rf_path <- function(row_id, model_frame) {
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
    vals = model_frame$prediction[row_id], 
    type = "path",
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
