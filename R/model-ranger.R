# Model parser ------------------------------------
get_ra_path <- function(node_id, tree){
  find <- node_id
  path <- node_id
  for(j in node_id:0){
    row <- tree[tree$nodeID == j, ]
    lc <- row["leftChild"] == find 
    lr <- row["rightChild"] == find
    if(is.na(lc)) lc <- FALSE
    if(is.na(lr)) lr <- FALSE
    dir <- NULL
    if(lc | lr)  {
      find <- j 
      path <- c(path, j)
    }
  }
  map2(
    path[1:length(path)-1],
    path[2:length(path)],
    ~ {
      rb <- tree[tree$nodeID == .y, ]
      lc <- rb["leftChild"] == .x
      lr <- rb["rightChild"] == .x
      if(is.na(lc)) lc <- FALSE
      if(is.na(lr)) lr <- FALSE
      if(lc) op <- "under"
      if(lr) op <- "over"
      list(
        type = "conditional",
        col = rb["splitvarName"][[1]],
        val = rb["splitval"][[1]],
        op = op
      )
    }
  )
}

get_ra_tree <- function(tree_no, model){
  tree <- ranger::treeInfo(model, tree_no)
  paths <- tree$nodeID[tree[, "terminal"]]
  map(
    paths,
    ~ {
      prediction <- tree$prediction[tree$nodeID == .x]
      if(is.factor(prediction)) prediction <- as.character(prediction)
      list(
        prediction = prediction,
        path = get_ra_path(.x, tree)
      )
    } 
  )  
}

get_ra_trees <- function(model){
  map(
    seq_len(model$num.trees),  
    ~ get_ra_tree(.x, model)
  )
}

#' @export
parse_model.ranger <- function(model){
  classes <- attr(model$terms, "dataClasses")
  pm <- list()
  pm$general$model <- "randomForest"
  pm$general$version <- 2
  pm$trees <- get_ra_trees(model)
  pm
}


# Fit formula -----------------------------------

#' @export
tidypredict_fit.ranger<- function(model){
  build_fit_formula_rf(model)
}