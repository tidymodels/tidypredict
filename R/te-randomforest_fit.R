#' @import rlang
#' @importFrom purrr map2
#' @importFrom purrr map
#' @importFrom purrr reduce
#' @import dplyr
te_randomforest_fit <- function(parsedmodel){
  
  
  paths <- parsedmodel %>%
    filter(type == "path")
  
  all_paths <- 1:nrow(paths) %>%
    map(~case_formula(paths$vals[.x], paths$field[.x], paths$operation[.x]))
  
  
  expr(case_when(!!! all_paths))
}

#' @import rlang
case_formula <- function(vals, field, operation){
  marker <- get_marker_regx()
  
  fields <- unlist(strsplit(field, marker))
  operations <- unlist(strsplit(operation, marker))
  
  f <- map2(fields, operations, function(x, y)parse_expr(paste0(x, " ", y)))
  
  f <- reduce(f, function(l, r) expr((!!l) & (!!r)))
  
  expr((!!! f) ~ !! vals)
}
