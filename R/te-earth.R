te_earth_fit <- function(parsedmodel) {
  pm <- parsedmodel[parsedmodel$type == "terms", ]
  pm <- pm[!is.na(pm$dirs), ]

  pm_list <- transpose(pm)

  pm_terms <- lapply(
    pm_list,
    function(x) {
      if (length(grep(":", x$field)) > 0) {
        fields <- strsplit(x$field, ":")
        s_fields <- syms(fields[[1]])
        field <- reduce(s_fields, function(x, y) expr(!!x * !!y))
      } else {
        field <- sym(x$field)
      }
      est <- x$estimate

      ret <- NULL

      if (x$labels == "(Intercept)") {
        ret <- expr(!!est)
      }
      if (x$dirs == 2) {
        ret <- expr(!!est * !!field)
      }
      if (x$dirs == 1) {
        ret <- expr(ifelse(!!field > !!x$cut, (!!field - !!x$cut) * !!est, 0))
      }
      if (x$dirs == -1) {
        ret <- expr(ifelse(!!field < !!x$cut, (!!x$cut - !!field) * !!est, 0))
      }

      ret
    }
  )

  fit <- reduce(pm_terms, function(x, y) expr(!!x + !!y))
  
  link <- parsedmodel$vals[parsedmodel$labels == "link"]
  
  if(length(link) > 0){
    
    assigned <- 0
    
    if (link == "identity") {
      assigned <- 1
    }
    
    if (link == "logit") {
      assigned <- 1
      fit <- expr(1 - 1 / (1 + exp(!! fit)))
    }
    
    if (link == "log") {
      assigned <- 1
      fit <- expr(exp(!! fit))
    }
    
    if (assigned == 0) {
      stop("Combination of family and link are not supported")
    }
  }
  fit
}
