te_earth_fit <- function(parsedmodel){
  
  pm <- parsedmodel[parsedmodel$type == "terms", ]
  pm <- pm[!is.na(pm$dirs), ]
  
  pm_list <- transpose(pm)
  
  pm_terms <- lapply(
    pm_list,
    function(x){
      
      if(length(grep(":", x$field)) > 0){
        fields <- strsplit(x$field, ":")
        s_fields <- syms(fields[[1]])
        field <- reduce(s_fields, function(x, y) expr(!! x * !! y))
      } else {
        field <- sym(x$field)
      }
      est <- x$estimate
      ret <- NULL
      if(x$labels == "(Intercept)") ret <- expr(!! est)
      if(x$dirs == 2) ret <- expr(!! est * !! field)
      if(x$dirs == 1) ret <- expr(ifelse(!! field > !! x$cut, (!! field - !! x$cut) * !! est, 0))
      if(x$dirs == -1) ret <- expr(ifelse(!! field < !! x$cut, (!! x$cut - !! field) * !! est, 0))
      ret 
    }
  )
  
  reduce(pm_terms, function(x, y) expr(!! x + !! y))
}