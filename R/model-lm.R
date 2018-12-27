# Predict ---------------------------------------

#' @export
tidypredict_fit.lm <- function(model) {
  parsedmodel <- parse_model(model)
  build_fit_formula(parsedmodel)
}

#' @export
tidypredict_fit.glm <- function(model) {
  parsedmodel <- parse_model(model)
  build_fit_formula(parsedmodel)
}

build_fit_formula <- function(parsedmodel) {
  parsed_f <- map(
    parsedmodel$terms,
    ~ {
      if (.x$is_intercept == 0) {
        cols <- map(
          .x$fields,
          ~ {
            f <- NULL
            if (.x$type == "ordinary") 
              f <- expr(!!sym(.x$col))
            if (.x$type == "conditional") 
              f <- expr(ifelse(!!sym(.x$col) == !!.x$val, 1, 0))
            if (.x$type == "operation") {
              if(.x$op == "morethan") 
                f <- expr(ifelse(!!sym(.x$col) > !!.x$val, !!sym(.x$col) - !!.x$val, 0))
              if(.x$op == "lessthan") 
                f <- expr(ifelse(!!sym(.x$col) < !!.x$val, !!.x$val - !!sym(.x$col), 0))
              
            }
            f
          }
        )
        cols <- reduce(cols, function(l, r) expr(!!l * !!r))
        expr((!!cols * !!.x$coef))
      } else {
        expr(!!.x$coef)
      }
    }
  )
  f <- reduce(parsed_f, function(l, r) expr(!!l + !!r))

  if (parsedmodel$general$is_glm == 1) {
    link <- parsedmodel$general$link
    assigned <- 0
    if (link == "identity") {
      assigned <- 1
    }
    if (link == "logit") {
      assigned <- 1
      f <- expr(1 - 1 / (1 + exp(!!f)))
    }
    if (link == "log") {
      assigned <- 1
      f <- expr(exp(!!f))
    }
    if (assigned == 0) {
      stop("Combination of family and link are not supported")
    }
  }
  f
}

# Parse model --------------------------------------

#' @export
parse_model.lm <- function(model) parse_model_lm(model)

#' @export
parse_model.glm <- function(model) parse_model_lm(model)

parse_model_lm <- function(model) {
  acceptable_formula(model)

  coefs <- as.numeric(model$coefficients)
  labels <- names(model$coefficients)
  vars <- names(attr(model$terms, "dataClasses"))
  qr <- NULL
  if (!is.null(model$qr)) qr <- qr.solve(qr.R(model$qr))

  pm <- list()
  pm$general$model <- class(model)[[1]]
  pm$general$version <- 2
  pm$general$residual <- model$df.residual

  if (length(summary(model)$sigma^2) > 0) {
    pm$general$sigma2 <- summary(model)$sigma^2
  }
  if (!is.null(model$family$family)) {
    pm$general$family <- model$family$family
  }
  if (!is.null(model$family$link)) {
    pm$general$link <- model$family$link
  }

  pm$general$is_glm <- 0
  if (class(model)[[1]] == "glm") {
    pm$general$is_glm <- 1
  }

  terms <- map(
    seq_len(length(labels)),
    ~ {
      list(
        label = labels[.x],
        coef = coefs[.x],
        is_intercept = ifelse(labels[.x] == "(Intercept)", 1, 0),
        fields = parse_label_lm(labels[.x], vars),
        qr = parse_qr_lm(labels[.x], qr)
      )
    }
  )
  pm$terms <- terms
  pm
}

parse_label_lm <- function(label, vars) {
  all_items <- NULL
  items <- strsplit(label, "\\:")[[1]]
  for (i in seq_len(length(items))) {
    item <- list(
      type = "ordinary",
      col = items[i]
    )
    cat_match <- map_lgl(vars, ~ .x == substr(items[i], 1, nchar(.x)))
    if (any(cat_match) && vars[cat_match] != items[i]) {
      item <- list(
        type = "conditional",
        col = vars[cat_match],
        val = substr(items[i], nchar(vars[cat_match]) + 1, nchar(items[i])),
        op = "equal"
      )
    }
    all_items <- c(all_items, list(item))
  }
  all_items
}

parse_qr_lm <- function(label, qr) {
  qrs <- qr[label == rownames(qr) ]
  qrs <- set_names(
    as.list(qrs),
    paste0("qr_", 1:length(qrs))
  )
}
