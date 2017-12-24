#' @import rlang
#' @importFrom purrr map2
#' @importFrom purrr reduce
#' @import dplyr
te_fit_lm <- function(parsedmodel){
  
  coefs <- filter(parsedmodel, type == "categorical") 
  part1 <- map2(syms(coefs$labels), coefs$vals, 
                function(name, val) expr((!!name) == (!!val)))
  f <- map2(part1, coefs$estimate, 
            function(name, est) expr(ifelse(!!name, (!!est), 0)))
  
  coefs <- filter(parsedmodel, type == "continuous") 
  f <- c(f,map2(syms(coefs$labels), coefs$estimate, 
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

#' @import rlang
#' @import dplyr
te_fit_glm <- function(parsedmodel){
  
  fit <- te_fit_lm(parsedmodel)
  
  family <- pull(filter(parsedmodel, labels == "family"), vals)
  link <- pull(filter(parsedmodel, labels == "link"), vals)
  
  
  assigned <- 0
  
  if(family == "gaussian" && link == "identity"){
    assigned <- 1
  }
  
  if(family == "binomial" && link == "logit"){
    assigned <- 1
    fit <- expr(exp(!! fit) / (exp(!! fit) + 1))
  }
  
  if(assigned ==0){
    stop("Combination of family and link are not supported")
  }
  
  fit
}

