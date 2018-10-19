# Formula builder ---------------------------------------------------
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
  )
  path$split_point <- as.numeric(path$split_point)
  path$rowid <- seq_len(nrow(path))
  right <- path[path$operator == "right", ]
  if (nrow(right) > 0) {
    right <- split(right, right$rowid)
    right <- map(
      right, 
      ~ expr((!! sym(.x$field)) >= !! .x$split_point)
    )
  } else {
    right <- NULL
  }
  left <- path[path$operator == "left", ]
  if (nrow(left) > 0) {
    left <- split(left, left$rowid)
    left <- map(
      left, 
      ~ expr((!! sym(.x$field)) >= !! .x$split_point)
    )
  } else {
    left <- NULL
  }
  f <- c(right, left) %>%
    reduce(function(l, r) expr((!! l) & (!! r)))
  
  expr((!!! f) ~ !! vals)
}

# Model parser ------------------------------------------------------
#' @export
parse_model.ranger <- function(model) {
  
  all_trees <- map_df(
    seq_len(model$num.trees),
    ~ get_tree(model, .x)
  )
  
  add_row(
    all_trees, 
    labels = "model", vals = "ranger", type = "variable"
  )
}


get_path_ranger <- function(row_id, model_frame) {
  field <- NULL
  operator <- NULL
  split_point <- NULL
  current_val <- row_id
  
  for (get_path in row_id:1) {
    current <- model_frame[get_path, ]
    if (!is.na(current$leftchild)) {
      if (current$leftchild == current_val || current$rightchild == current_val) {
        field <- c(
          field,
          as.character(current$splitvarname)
        )
        operator <- c(
          operator,
          if (current$leftchild == current_val) "left" else "right"
        )
        split_point <- c(
          split_point,
          as.character(current$splitval)
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


get_tree <- function(model, tree){
  
  model_frame <- ranger::treeInfo(model, tree = tree)
  model_frame$rowid <- model_frame$nodeID
  colnames(model_frame) <- tolower(colnames(model_frame))
  
  all_paths <- model_frame[is.na(model_frame$splitval), ]
  all_paths <- all_paths$rowid
  all_paths <- map_df(
    all_paths, 
    ~ get_path_ranger(.x, model_frame)
  )
  tidy <- model_frame[is.na(model_frame$splitval), ]
  tidy <- cbind(
    labels = paste0("tree_", tree),
    type = "path",
    estimate = tidy$prediction,
    all_paths,
    vals = ""
  )
  tidy$labels <- as.character(tidy$labels)
  if(is.factor(tidy$vals)) tidy$vals <- as.character(tidy$vals)
  if(is.factor(tidy$estimate)) tidy$estimate <- as.character(tidy$estimate)  
  tidy$type <- as.character(tidy$type)
  as.tibble(tidy)
}

# Fit method ------------------------------------------------------
#' @export
tidypredict_fit.ranger <- function(model) {
  parsedmodel <- parse_model(model)
  te_ranger_fit(parsedmodel)
}
