# Model parser -------------------------------------

get_rf_path <- function(row_id, tree, columns){
  find <- row_id
  path <- row_id
  for(j in row_id:1){
    dir <- NULL
    if(tree[j, "left daughter"] == find | tree[j, "right daughter"] == find)  {
      find <- j
      path <- c(path, j)
    }
  }
  map2(
    path[1:length(path)-1],
    path[2:length(path)],
    ~ {
      rb <- tree[.y, ]
      if(rb["left daughter"] == .x) op <- "under"
      if(rb["right daughter"] == .x) op <- "over"
      list(
        type = "conditional",
        col = columns[rb["split var"]],
        val = rb["split point"][[1]],
        op = op
      )
    }
  )
}

get_rf_tree <- function(tree_no, model){
  predictions <- model$classes
  term_labels <- attr(model$terms, "term.labels")
  tree <- randomForest::getTree(model, tree_no)
  paths <- seq_len(nrow(tree))[tree[, "status"] == -1]
  map(
    paths,
    ~ {
      list(
        prediction = predictions[tree[.x, "prediction"]],
        path = get_rf_path(.x, tree, term_labels)
      )
    } 
  )  
}

get_rf_trees <- function(model){
  map(
    seq_len(model$ntree),  
    ~ get_rf_tree(.x, model)
  )
}

#' @export
parse_model.randomForest <- function(model){
  classes <- attr(model$terms, "dataClasses")
  pm <- list()
  pm$general$model <- "randomForest"
  pm$general$version <- 2
  pm$trees <- get_rf_trees(model)
  pm
}

# Fit model -----------------------------------------------

get_rf_case <- function(path, prediction){
  cl <- map(
    path, 
    ~{
      if(.x$op == "over") i <- expr(!! sym(.x$col) >= !! .x$val)
      if(.x$op == "under") i <- expr(!! sym(.x$col) < !! .x$val)
      i
    }
  )
  cl <- reduce(cl, function(x, y) expr(!! x & !! y))
  expr(!! cl ~ !! prediction)
}

get_rf_case_tree <- function(tree_no, parsedmodel){
  map(
    parsedmodel$trees[[tree_no]],
    ~ get_rf_case(.x$path, .x$prediction)
  )
}

#' @export
tidypredict_fit.randomForest <- function(model){
  parsedmodel <- parse_model(model)
  map(
    seq_len(length(parsedmodel$trees)),
    ~ expr(case_when(!!! get_rf_case_tree(.x, parsedmodel)))
  )
}