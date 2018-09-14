
te_earth_fit <- function(parsedmodel){
  fit <- parsedmodel[parsedmodel$type == "terms", ]
  fit <- fit[!is.na(fit$dirs), ]
  fit <- transpose(fit)
  fit <- lapply(
    fit,
    function(x){
      ret <- NULL
      if(x$labels == "(Intercept)") ret <- expr(!! x$estimate)
      if(x$dirs == 2) ret <- expr(!! x$estimate * !! sym(x$field))
      if(x$dirs == 1) ret <- expr(ifelse(!! sym(x$field) > !! x$cut, (!! sym(x$field) - !! x$cut) * !! x$estimate, 0))
      if(x$dirs == -1) ret <- expr(ifelse(!! sym(x$field) < !! x$cut, (!! x$cut - !! sym(x$field)) * !! x$estimate, 0))
      ret 
    }
  )
  reduce(fit, function(x, y) expr(!! x + !! y))
  
}
