# Model parser -------------------------------------

get_xgb_path <- function(row_id, tree){
  find <- row_id
  path <- row_id 
  for(j in row_id:1){
    dir <- NULL
    if(tree[j, "Yes"] %in% find | tree[j, "No"] %in% find) {
      find <- j
      path <- c(path, j)
    }
  }
  purrr::map2(
    path[1:length(path)-1],
    path[2:length(path)],
    ~ {
      rb <- tree[.y, ]
      if(rb["Yes"] %in% .x) {
        op <- "under"
        missing <- rb["Missing"] %in% rb["Yes"]
      }
      if(rb["No"]  %in% .x)  {
        op <- "over"
        missing <- rb["Missing"] %in% rb["No"]
      }
      list(
        type = "conditional",
        col = rb$feature_name,
        val = rb$Split,
        op = op,
        missing = missing
      )
    }
  )
}

get_xgb_tree <- function(tree){
  paths <- seq_len(nrow(tree))[tree[, "Feature"] == "Leaf"]
  map(
    paths,
    ~ {
      list(
        prediction = tree[.x, "Quality", drop = TRUE],
        path = get_xgb_path(.x, tree)
      )
    } 
  )  
}

get_xgb_trees <- function (model) {
  UseMethod("get_xgb_trees", model)
}

get_xgb_trees.xgb.Booster <- function(model) {
  xgb_dump_text_with_stats <- xgb.dump(model, dump_format = "text", with_stats = TRUE)
  feature_names <- model$feature_names
  
  get_xgb_trees.character(xgb_dump_text_with_stats, feature_names)
}

get_xgb_trees.character <- function(xgb_dump_text_with_stats, feature_names) {
  feature_names_tbl <- tibble::enframe(feature_names, "Feature", "feature_name") %>% dplyr::mutate(Feature = as.character(Feature - 1))
  
  trees <- xgb.model.dt.tree(text = xgb_dump_text_with_stats) %>%
    dplyr::left_join(feature_names_tbl, by = "Feature") %>%
    dplyr::mutate_at(dplyr::vars(Yes, No, Missing), ~stringr::str_replace(., "^.*-", "")) %>%
    dplyr::mutate_at(dplyr::vars(Yes, No, Missing), ~as.integer(.) + 1) %>% # xxgboost is 0-indexed
    dplyr::group_by(Tree) %>%
    tidyr::nest(.key = "tree_data")
  
  purrr::map(trees$tree_data, get_xgb_tree)
}

#' @export
parse_model.xgb.Booster <- function(model){
  pm <- list()
  pm$general$model <- "xgb.Booster"
  pm$general$niter <- model$niter
  pm$general$params <- model$params
  pm$general$feature_names <- model$feature_names
  pm$general$nfeatures <- model$nfeatures
  pm$general$version <- 1
  pm$trees <- get_xgb_trees(model)
  pm
}

# Fit model -----------------------------------------------

get_xgb_case <- function(path, prediction){
  cl <- map(
    path, 
    ~{
      if(.x$op == "over"  & .x$missing)  i <- expr((!! sym(.x$col) >= !! .x$val | is.na(!! sym(.x$col))))
      if(.x$op == "under" & .x$missing)  i <- expr((!! sym(.x$col) <  !! .x$val | is.na(!! sym(.x$col))))
      if(.x$op == "over"  & !.x$missing)  i <- expr(!! sym(.x$col) >= !! .x$val)
      if(.x$op == "under" & !.x$missing) i <- expr(!! sym(.x$col) <  !! .x$val)
      i
    }
  )
  cl <- if(length(cl) > 0) reduce(cl, function(x, y) expr(!! x & !! y)) else TRUE
  expr(!! cl ~ !! prediction)
}

get_xgb_case_tree <- function(tree_no, parsedmodel){
  map(
    parsedmodel$trees[[tree_no]],
    ~ get_xgb_case(.x$path, .x$prediction)
  )
}

build_fit_formula_xgb <- function(parsedmodel){
  
  f <- map(
    seq_len(length(parsedmodel$trees)),
    ~ expr(case_when(!!! get_xgb_case_tree(.x, parsedmodel)))
  )
  
  # additive model
  f <- purrr::reduce(f, ~expr(!!.x + !!.y), .init = expr(0))
  
  base_score <- parsedmodel$general$params$base_score
  if(is.null(base_score)) base_score <- 0.5
  
  objective <- parsedmodel$general$params$objective
    assigned <- 0
    if(is.null(objective)) {
      assigned <- 1
      f <- expr(!!f + !! base_score)
      warning("If the objective is a custom function, please explicitly apply it to the output.")
    } else if (objective %in% c("reg:linear")) {
      assigned <- 1
      f <- expr(!!f + !! base_score)
    } else if (objective %in% c("binary:logitraw")) {
      assigned <- 1
    } else if (objective %in% c("binary:logistic", "reg:logistic")) {
      assigned <- 1
      f <- expr(1 - 1 / (1 + exp( !!f)))
    }
    if (assigned == 0) {
      stop("Only objectives 'binary:logistic', 'reg:linear', 'reg:logistic', 'binary:logitraw' are supported yet.")
    }
  f
}

#' @export
tidypredict_fit.xgb.Booster <- function(model){
  parsedmodel <- parse_model(model)
  build_fit_formula_xgb(parsedmodel)
}


