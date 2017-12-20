
#' @import rlang
#' @importFrom purrr map2
#' @importFrom purrr reduce
#' @import dplyr
#' @export
te_formula_lm <- function(parsedmodel, offset = NULL){
  
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
  
  if(!is.null(offset)){
    f <- c(f, offset)
  }
  
  
  reduce(f, function(l, r) expr((!!l) + (!!r)))
  
}
