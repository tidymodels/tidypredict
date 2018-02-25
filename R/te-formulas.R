strip_factor <- function(x) gsub("factor\\((.+)\\)", "\\1", x)

te_fit_lm <- function(parsedmodel) {
  labels <- parsedmodel %>%
    filter(labels == "labels") %>%
    as.character()

  labels <- labels[4:length(labels)]
  labels <- c("estimate", labels)
  all_terms <- parsedmodel %>%
    filter(.data$type == "term") %>%
    select(-.data$type, -.data$labels)

  selection <- which(labels != "NA")
  all_terms <- all_terms[, which(labels != "NA")]
  colnames(all_terms) <- labels[which(labels != "NA")]


  f <- seq_len(nrow(all_terms)) %>%
    map(~{
      vars <- strip_factor(colnames(all_terms))
      vals <- as.character(all_terms[.x, ])

      estimate <- vals[vars == "estimate"]
      estimate <- expr(!! as.numeric(estimate))

      reg <- vars[vals == "{{:}}" & !is.na(vals) & vars != "estimate"]
      reg <- expr(!! syms(reg))

      field <- vars[vals != "{{:}}" & !is.na(vals) & vars != "estimate"]
      val <- vals[vals != "{{:}}" & !is.na(vals) & vars != "estimate"]
      ie <- map2(syms(field), val, function(x, y) expr((!! x) == (!! y)))
      ie <- map(ie, function(x) expr(ifelse(!! x, 1, 0)))
      set <- c(reg, ie, estimate)
      reduce(set, function(l, r) expr((!!! l) * (!!! r)))
    })

  offset <- filter(parsedmodel, labels == "offset")
  if (nrow(offset) > 0) {
    f <- c(f, sym(offset$vals))
  }

  reduce(f, function(l, r) expr((!! l) + (!! r)))
}

te_fit_glm <- function(parsedmodel) {
  fit <- te_fit_lm(parsedmodel)

  family <- pull(filter(parsedmodel, labels == "family"), .data$vals)
  link <- pull(filter(parsedmodel, labels == "link"), .data$vals)

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

  fit
}
