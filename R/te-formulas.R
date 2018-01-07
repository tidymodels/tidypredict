#' @import rlang
#' @importFrom purrr map2
#' @importFrom purrr reduce
#' @import dplyr
te_fit_lm <- function(parsedmodel) {
  
  labels <- parsedmodel %>%
    filter(labels == "labels") %>%
    as.character()
  
  labels <- labels[4:length(labels)]
  labels <- c("estimate", labels)
  all_terms <- parsedmodel %>%
    filter(type == "term") %>%
    select(- type, -labels)
  
  selection <- which(labels != "NA")
  all_terms <- all_terms[, which(labels != "NA")]
  colnames(all_terms) <- labels[which(labels != "NA")]
  
  t <- seq_len(nrow(all_terms)) %>%
    map(~all_terms[.x, ] %>%
          gather() %>%
          filter(!is.na(value) )
    )
  
  
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
  
  offset <- filter(parsedmodel, labels == "offset")
  if (nrow(offset) > 0) {
    f <- c(f, sym(offset$vals))
  }
  
  reduce(f, function(l, r) expr((!! l) + (!! r)))
}

#' @import rlang
#' @import dplyr
te_fit_glm <- function(parsedmodel) {
  fit <- te_fit_lm(parsedmodel)

  family <- pull(filter(parsedmodel, labels == "family"), vals)
  link <- pull(filter(parsedmodel, labels == "link"), vals)

  assigned <- 0

  if (family == "gaussian" && link == "identity") {
    assigned <- 1
  }

  if (family == "binomial" && link == "logit") {
    assigned <- 1
    fit <- expr(exp(!! fit) / (exp(!! fit) + 1))
  }

  if (assigned == 0) {
    stop("Combination of family and link are not supported")
  }

  fit
}
