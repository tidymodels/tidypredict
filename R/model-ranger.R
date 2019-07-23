# Model parser ------------------------------------
get_ra_path <- function(node_id, tree, default_op = TRUE) {
  find <- node_id
  path <- node_id
  for (j in node_id:0) {
    row <- tree[tree$nodeID == j, ]
    lc <- row["leftChild"][[1]] == find
    lr <- row["rightChild"][[1]] == find
    if (is.na(lc)) lc <- FALSE
    if (is.na(lr)) lr <- FALSE
    dir <- NULL
    if (lc | lr) {
      find <- j
      path <- c(path, j)
    }
  }
  map2(
    path[1:length(path) - 1],
    path[2:length(path)],
    ~ {
      rb <- tree[tree$nodeID == .y, ]
      lc <- rb["leftChild"] == .x
      lr <- rb["rightChild"] == .x
      if(is.na(rb["splitval"][[1]])) {
        if (lc) op <- "in"
        if (lr) op <- "not-in"
        vals <- strsplit(as.character(rb["splitclass"][[1]]), ", ")[[1]]
        list(
          type = "set",
          col = as.character(rb["splitvarName"][[1]]),
          vals = map(vals, ~.x),
          op = op
        )
      } else {
        if (default_op) {
          if (lc) op <- "less"
          if (lr) op <- "more-equal"
        } else {
          if (lc) op <- "less-equal"
          if (lr) op <- "more"
        }
        list(
          type = "conditional",
          col = as.character(rb["splitvarName"][[1]]),
          val = rb["splitval"][[1]],
          op = op
        )
      }
    }
  )
}

get_ra_tree <- function(tree_no, model) {
  tree <- ranger::treeInfo(model, tree_no)
  paths <- tree$nodeID[tree[, "terminal"]]
  map(
    paths,
    ~ {
      prediction <- tree$prediction[tree$nodeID == .x]
      if (!is.null(prediction)) {
        if (is.factor(prediction)) prediction <- as.character(prediction)  
        list(
          prediction = prediction,
          path = get_ra_path(.x, tree, TRUE)
        )        
      } else {
        preds <- map_lgl(colnames(tree), ~"pred." == substr(.x, 1, 5))
        preds_table <- tree[tree$nodeID == .x, preds]
        predictors <- map_chr(colnames(preds_table), ~substr(.x, 6, nchar(.x)))
        colnames(preds_table) <- predictors
        predictions <- map(preds_table, ~.x)
        max_pred <- map_lgl(predictions, ~.x == max(map_dbl(predictions, ~.x)))
        prediction <- names(predictions)[max_pred]
        prediction <- prediction[[1]]
        prob <- predictions[max_pred]
        prob <- prob[[1]]
        predictions <- imap(predictions, ~list(pred = .y, prob = .x))
        list(
          prediction = prediction,
          prob = prob,
          probs = predictions,
          path = get_ra_path(.x, tree, TRUE)
        )       
      }

    }
  )
}

get_ra_trees <- function(model) {
  map(
    seq_len(model$num.trees),
    ~ get_ra_tree(.x, model)
  )
}

#' @export
parse_model.ranger <- function(model) {
  classes <- attr(model$terms, "dataClasses")
  pm <- list()
  pm$general$model <- "ranger"
  pm$general$type <- "tree"
  pm$general$version <- 2
  pm$trees <- get_ra_trees(model)
  as_parsed_model(pm)
}

# Fit formula -----------------------------------

#' @export
tidypredict_fit.ranger <- function(model) {
  parsedmodel <- parse_model(model)
  build_fit_formula_rf(parsedmodel)
}
