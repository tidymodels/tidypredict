#' @import rlang
#' @importFrom purrr map2
#' @importFrom purrr map
#' @importFrom purrr reduce
#' @import dplyr
xr <- function(parsedmodel, current, res.var){
  
  parsedmodel <- parsedmodel %>% 
    filter(type != "variable") %>%
    mutate(current = current,
           sym_labels = syms(labels)) 
  
  coefs <- filter(parsedmodel, type == "categorical") 
  part1 <- map2(coefs$sym_labels, coefs$vals, 
                function(name, val) expr((!!name) == (!!val)))
  
  f <- map2(part1, coefs$current, 
            function(name, est) expr((ifelse(!!name, (!!est) , 0))))
  
  
  coefs <- filter(parsedmodel, type == "continuous") 
  f <- c(f,map2(coefs$sym_labels, coefs$current, 
                function(name, val) expr(((!!name) * (!!val)))
  ))
  
  intercept <- parsedmodel %>%
    filter(labels == "(Intercept)") %>%
    pull(current)
  
  
  if(length(intercept) > 0){
    f <- c(f, expr(((!! intercept))))
  }
  
  f <- reduce(f, function(l, r) expr((!!l) + (!!r)))
  
  f <- expr((!! f) * (!! f) * (!! res.var))
  
  f
  
}

#' @import rlang
#' @importFrom purrr map2
#' @importFrom purrr reduce
#' @import dplyr
te_interval_lm <- function(parsedmodel, interval = 0.95){

  res.var <- parsedmodel %>%
    filter(labels == "sigma2") %>%
    pull(vals) %>%
    as.numeric()
  
  res <- parsedmodel %>%
    filter(labels == "residual") %>%
    pull(vals) %>%
    as.numeric()
  
  qr <- parsedmodel %>%
    filter(type != "variable") %>%
    select(starts_with("qr_"))

  rank <- parsedmodel %>%
    filter(type != "variable") %>%
    nrow()
  
  xrinv<- 1:rank %>%
    map(~xr(parsedmodel, current = pull(qr[, .x]), res.var = res.var))
  
  ip <-  reduce(xrinv, function(l, r) expr((!!l) + (!!r)))
  
  tfrac <- qt(1-(1 - interval)/2, res)
  
  expr((!!tfrac ) * sqrt((!!ip) + (!! res.var)))
  
}

#' @import rlang
#' @import dplyr
te_interval_glm <- function(parsedmodel, interval = 0.95){
  
  intervals <- te_interval_lm(parsedmodel, interval)
  
  family <- pull(filter(parsedmodel, labels == "family"), vals)
  link <- pull(filter(parsedmodel, labels == "link"), vals)
  
  assigned <- 0
  
  if(family == "gaussian" && link == "identity"){
    assigned <- 1
  }
  
  if(assigned ==0){
    stop("Combination of family and link are not supported for prediction intervals")
  }

  intervals
}

