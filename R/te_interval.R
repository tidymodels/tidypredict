

xr <- function(parsedmodel, current, res.var){
  
  parsedmodel <- parsedmodel %>% 
    mutate(current = current) %>%
    filter(current != 0)
  
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
#' @export
te_interval_lm <- function(model, interval = 0.95){
  
  parsedmodel <- parsemodel(model)
  
  res.var <- summary(model)$sigma^2
  
  qr <- qr.solve(qr.R(model$qr))
  
  xrinv<- 1:model$rank %>%
    map(~xr(parsedmodel, current = qr[, .x], res.var = res.var))
  
  ip <-  reduce(xrinv, function(l, r) expr((!!l) + (!!r)))
  
  tfrac <- qt(1-(1 - interval)/2, model$df.residual)
  
  expr((!!tfrac ) * sqrt((!!ip) + (!! res.var)))
  
}




