#' @import rlang
#' @importFrom purrr map2
#' @importFrom purrr reduce
#' @import dplyr
#' @export
te_fit_lm <- function(parsedmodel){
  
  coefs <- filter(parsedmodel, type == "categorical") 
  part1 <- map2(coefs$sym_labels, coefs$vals, 
                function(name, val) expr((!!name) == (!!val)))
  f <- map2(part1, coefs$estimate, 
            function(name, est) expr(ifelse(!!name, (!!est), 0)))
  
  coefs <- filter(parsedmodel, type == "continuous") 
  f <- c(f,map2(coefs$sym_labels, coefs$estimate, 
                function(name, val) expr((!!name) * (!!val))))
  
  intercept <- filter(parsedmodel, labels == "(Intercept)")
  
  if(nrow(intercept) > 0){
    f <- c(f, intercept$estimate)
  }
  
  
  offset <- filter(parsedmodel, labels == "offset")
  if(nrow(offset) > 0){
    f <- c(f, sym(offset$vals))
  }
  
  reduce(f, function(l, r) expr((!!l) + (!!r)))
  
}