#' @import rlang
#' @importFrom purrr map2
#' @importFrom purrr map
#' @importFrom purrr reduce
#' @import dplyr
xr <- function(parsedmodel, qr_field, res.var) {
  qr_field <- parse_expr(qr_field)
  
  labels <- parsedmodel %>%
    filter(labels == "labels") %>%
    as.character()
  
  labels <- labels[4:length(labels)]
  labels <- c("estimate", labels)
  
  
  all_terms <- parsedmodel %>%
    mutate(estimate = !! qr_field) %>%
    filter(type == "term",
           estimate != 0) %>%
    select(- type, -labels) 
  
  selection <- which(labels != "NA")
  all_terms <- all_terms[, which(labels != "NA")]
  colnames(all_terms) <- labels[which(labels != "NA")]
  
  
  f <- seq_len(nrow(all_terms)) %>%
    map(~{
      vars <- colnames(all_terms)
      vals <- as.character(all_terms[.x, ])
      
      estimate <- vals[vars == "estimate"]
      estimate <- expr(!! as.numeric(estimate))
      
      reg <- vars[vals == "{{:}}" & !is.na(vals) & vars != "estimate"]
      reg <- expr(!! syms(reg))
      
      field <- vars[vals != "{{:}}" & !is.na(vals) & vars != "estimate"]
      val <-  vals[vals != "{{:}}" & !is.na(vals) & vars != "estimate"]
      ie <- map2(syms(field), val, function(x, y) expr((!!x) == (!!y)))
      ie <- map(ie, function(x) expr(ifelse(!!x, 1, 0)))
      set <- c(reg, ie, estimate)
      reduce(set, function(l, r) expr((!!! l) * (!!! r)))
    } )
  
  f <- reduce(f, function(l, r) expr((!! l) + (!! r)))
  
  f <- expr((!! f) * (!! f) * (!! res.var))
  
  f
}

#' @import rlang
#' @importFrom purrr map2
#' @importFrom purrr reduce
#' @import dplyr
te_interval_lm <- function(parsedmodel, interval = 0.95) {
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

  xrinv <- colnames(qr) %>% 
    map(~xr(parsedmodel, .x, res.var)) %>%
    flatten()

  ip <- reduce(xrinv, function(l, r) expr((!! l) + (!! r)))

  tfrac <- qt(1 - (1 - interval) / 2, res)

  expr((!! tfrac) * sqrt((!! ip) + (!! res.var)))
}

#' @import rlang
#' @import dplyr
te_interval_glm <- function(parsedmodel, interval = 0.95) {
  intervals <- te_interval_lm(parsedmodel, interval)

  family <- pull(filter(parsedmodel, labels == "family"), vals)
  link <- pull(filter(parsedmodel, labels == "link"), vals)

  assigned <- 0

  if (family == "gaussian" && link == "identity") {
    assigned <- 1
  }

  if (assigned == 0) {
    stop("Combination of family and link are not supported for prediction intervals")
  }

  intervals
}
