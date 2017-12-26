#' Converts an R model object into a table
#'
#'It parses a fitted R model's structure to extract all of the necessary
#'components to pass along to the functions that create the actual 
#'Tidy Eval formula that dplyr can read.  It is currently able to parse
#'lm() and glm() models.
#'
#' @param model An R model object
#'
#' @examples
#' 
#' df <- data.frame(x = c(1, 2, 5, 6 ,6), y = c(2, 3, 6, 5, 4))
#' model <- lm(x ~ y, df)
#' parsemodel(model)
#'
#' @export
parsemodel <- function(model){
  UseMethod("parsemodel")
}
#' @export
parsemodel.lm <- function(model) parsemodel_lm(model)

#' @export
parsemodel.glm <- function(model) parsemodel_lm(model)

#' @import dplyr
#' @importFrom tibble tibble
add_variable <- function(df, labels, vals){
  df %>%
    bind_rows(tibble(
      labels = !!labels,
      vals = as.character(!! vals),
      type = "variable"
    ))
}

#' @import rlang
#' @importFrom purrr map2
#' @importFrom purrr map
#' @importFrom purrr reduce
#' @import dplyr
#' @importFrom tibble tibble
#' @importFrom tibble rowid_to_column
#' @importFrom tibble rownames_to_column
#' @importFrom utils head
#' @importFrom stats predict 
#' @importFrom stats qt
parsemodel_lm <- function(model){
  
  acceptable_formula(model)
  
  terms <- model$terms
  
  labels <- attr(terms, "term.labels")
  
  tidy <- tibble(labels)
  
  xl <- model$xlevels
  
  if(length(xl) > 0){
    xlevels <- names(xl) %>%
      map({~tibble(
        labels = .x,
        vals = as.character(xl[[.x]])) %>%
          rowid_to_column("row")}
      ) %>%
      bind_rows() %>%
      filter(row > 1) %>%
      select(-row)
    
    tidy <- tidy %>%
      left_join(xlevels, by = "labels") 
  }
  
  i <- attr(terms, "intercept")
  if(!is.null(i)){
    tidy <-
      tibble(
        labels = "(Intercept)",
        vals = ""
      ) %>%
      bind_rows(tidy) 
  }
  
  tidy <- tidy %>% 
    mutate(
      vals = ifelse(is.na(vals), "", vals),
      type = case_when(
        labels == "(Intercept)" ~ "intercept",
        vals == "" ~ "continuous",
        vals != "" ~ "categorical",
        TRUE ~ "error")
    ) 
  
  tidy <- add_variable(tidy, labels = "model", vals = class(model)[[1]])
  tidy <- add_variable(tidy, labels = "residual", vals = model$df.residual)
  
  if(length(summary(model)$sigma^2) > 0){
    tidy <- add_variable(tidy, labels = "sigma2", vals = summary(model)$sigma^2)
  } 
  
  if(!is.null(model$family$family)){
    tidy <- add_variable(tidy, labels = "family", vals = model$family$family)
  }

  if(!is.null(model$family$link)){
    tidy <- add_variable(tidy, labels = "link", vals = model$family$link)
  }
  
  offset <- model$call$offset
  if(!is.null(offset)){
    tidy <- tidy %>%
      bind_rows(tibble(
        labels = "offset", 
        vals = as.character(offset),
        type = "variable")        
      )}
  
  coef <- summary(model)$coefficients  %>%
    as.data.frame() %>%
    rownames_to_column("coef_labels") %>%
    select(coef_labels, 
           estimate = Estimate) 
  
  res.var <- summary(model)$sigma^2
  
  qr <- qr.solve(qr.R(model$qr)) %>%
    as.data.frame() %>%
    rownames_to_column()
  
  colnames(qr) <- c("coef_labels", paste0("qr_", 1:nrow(qr)))
  
  tidy <- tidy %>%
    mutate(coef_labels = paste0(labels, vals)) %>%
    full_join(coef, by = "coef_labels") %>%
    full_join(qr, by = "coef_labels") %>%
    select(- coef_labels) 
  
  if(any(is.na(tidy$labels))){
    stop("Error parsing the model")
  }
  
  tidy
}

#' @import rlang
#' @importFrom purrr map2
#' @importFrom purrr map
#' @importFrom purrr reduce
#' @import dplyr
#' @export
parsemodel.randomForest <- function(model){
  
  model_tree <- tree::tree(model)
  
  model_frame <- model_tree$frame
  
  model_frame$rowid <- rownames(model_frame)
  
  rm(model_tree)
  
  model_frame <- model_frame %>%
    select(var, n, dev, rowid)  %>%
    bind_cols(as.data.frame(model_frame$splits)) %>%
    bind_cols(as.data.frame(model_frame$yprob)) %>%
    mutate(rowid = as.numeric(rowid),
           cutleft = as.character(cutleft),
           cutright = as.character(cutright),
           var = as.character(var),
           parent = floor(rowid / 2),
           from_left = rowid / 2 == parent)
  
  result_vars <-model_frame[, model$classes]
  
  response <- 1:nrow(result_vars) %>%
    map(~which.max(result_vars[.x,])) %>%
    map(~attr(.x, "names")) %>%
    as.character()
  
  percent <- 1:nrow(result_vars) %>%
    map(~result_vars[.x,response[.x]]) %>%
    as.numeric()
  
  model_frame <- model_frame %>%
    mutate(response = response,
           percent = percent)
  
  rm(result_vars)
  rm(response)
  rm(percent)
  
  all_paths <- model_frame %>%
    filter(cutleft == "") %>%
    pull(rowid) %>%
    map(~get_path(.x, model_frame)) %>%
    bind_rows()
  
  parser <- model_frame %>%
    filter(cutleft == "") %>%
    rownames_to_column() %>%
    mutate(labels = paste0("path-", rowname),
           vals = response, 
           type = "path", 
           estimate = percent) %>%
    select(labels, vals, type, estimate) %>%
    bind_cols(all_paths)  %>%
    bind_rows(tibble(
      labels = "model",
      vals = "randomForest",
      type = "variable",
      estimate = NA,
      field = NA,
      operation = NA
    )) 
  
  parser
  
}

get_marker <- function()"{:}"
get_marker_regx <- function()"\\{\\:\\}"



get_path <- function(row_id, model_frame){
  field <- NULL
  operation <- NULL
  for(get_path in 1:nrow(model_frame)){
    if(row_id == 0){
      path <- tibble(
        field = paste0(field, collapse = get_marker()),
        operation = paste0(operation, collapse = get_marker())
      )
      break
    } 
    current <- filter(model_frame, rowid == row_id)
    if(current$cutleft != ""){
      field <- c(
        field, 
        current$var)
      
      operation <- c(
        operation, 
        ifelse(current_side, current$cutleft, current$cutright))
      
    } 
    
    row_id <- current$parent
    current_side <- current$from_left
  }
  path
  
}