# Formula builder ---------------------------------------------------
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

te_interval_lm <- function(parsedmodel, interval = 0.95) {
  res.var <- parsedmodel %>%
    filter(labels == "sigma2") %>%
    pull(.data$vals) %>%
    as.numeric()
  
  res <- parsedmodel %>%
    filter(.data$labels == "residual") %>%
    pull(.data$vals) %>%
    as.numeric()
  
  qr <- parsedmodel %>%
    filter(.data$type == "term") %>%
    select(starts_with("qr_"))
  
  xrinv <- colnames(qr) %>%
    map(~xr(
      parsedmodel = parsedmodel,
      qr_field = .x,
      res.var = res.var
    ))
  
  ip <- reduce(xrinv, function(l, r) expr((!! l) + (!! r)))
  
  tfrac <- qt(1 - (1 - interval) / 2, res)
  
  expr((!! tfrac) * sqrt((!! ip) + (!! res.var)))
}

xr <- function(parsedmodel = NULL, qr_field = NULL, res.var = NULL) {
  qr_field <- parse_expr(qr_field)
  
  labels <- parsedmodel %>%
    filter(.data$labels == "labels") %>%
    as.character()
  
  labels <- labels[4:length(labels)]
  labels <- c("estimate", labels)
  
  
  all_terms <- parsedmodel %>%
    mutate(estimate = !! qr_field) %>%
    filter(
      .data$type == "term",
      .data$estimate != 0
    ) %>%
    select(-.data$type, -.data$labels)
  
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
      val <- vals[vals != "{{:}}" & !is.na(vals) & vars != "estimate"]
      ie <- map2(syms(field), val, function(x, y) expr((!! x) == (!! y)))
      ie <- map(ie, function(x) expr(ifelse(!! x, 1, 0)))
      set <- c(reg, ie, estimate)
      reduce(set, function(l, r) expr((!!! l) * (!!! r)))
    })
  
  f <- reduce(f, function(l, r) expr((!! l) + (!! r)))
  
  f <- expr((!! f) * (!! f) * (!! res.var))
  
  f
}

# Model parser ------------------------------------------------------
#' @export
parse_model.lm <- function(model) parse_model_lm(model)

parse_model_lm <- function(model) {
  acceptable_formula(model)
  
  var_labels <- names(attr(model$terms, "dataClasses"))
  if (attr(model$terms, "response") == 1) var_labels <- var_labels[2:length(var_labels)]
  
  vars <- tibble(var = var_labels)
  
  xl <- model$xlevels
  if (length(xl) > 0) {
    xl_df <- seq_along(xl) %>%
      map_df(~tibble(
        var = names(xl[.x]),
        vals = xl[[.x]]
      ))
    vars <- vars %>%
      left_join(xl_df, by = "var") %>%
      mutate(fullname = paste0(.data$var, ifelse(is.na(.data$vals), "", .data$vals)))
  } else {
    vars <- vars %>%
      mutate(fullname = .data$var)
  }
  
  co <- model$coefficients
  
  est <- names(co) %>%
    map(~strsplit(.x, ":"))
  
  est_df <- seq_along(est) %>%
    map_df(~tibble(
      coefno = .x,
      fullname = est[[.x]][[1]]
    ))
  
  all_vals <- est_df %>%
    left_join(vars, by = "fullname") %>%
    mutate(vals = ifelse(.data$fullname == .data$var, "{{:}}", .data$vals)) %>%
    filter(!is.na(.data$var)) %>%
    filter(!is.na(.data$vals)) %>%
    select(-.data$fullname) %>%
    group_by(.data$coefno) %>%
    spread(.data$var, .data$vals)
  
  new_vals <- as_list(colnames(all_vals))
  names(new_vals) <- colnames(all_vals)
  
  all_vals <- as_tibble(new_vals) %>%
    mutate(coefno = 0L) %>%
    bind_rows(all_vals)
  
  colnames(all_vals) <- c("coefno", paste0("field_", (2:length(all_vals)) - 1))
  
  tidy <- as_tibble(model$coefficients) %>%
    rownames_to_column("labels") %>%
    rowid_to_column("coefno") %>%
    rename(estimate = .data$value) %>%
    mutate(type = "term") %>%
    bind_rows(tibble(
      coefno = 0,
      labels = "labels",
      estimate = 0,
      type = "variable"
    )) %>%
    left_join(all_vals, by = "coefno")
  
  qr <- qr.solve(qr.R(model$qr)) %>%
    as.data.frame() %>%
    rownames_to_column()
  
  colnames(qr) <- c("coef_labels", paste0("qr_", seq_len(nrow(qr))))
  
  cf <- as_list(c("labels", rep(NA, length(qr) - 1)))
  names(cf) <- names(qr)
  
  qr <- qr %>%
    bind_rows(as_tibble(cf))
  
  tidy <- tidy %>%
    bind_cols(qr) %>%
    mutate(label_match = .data$coef_labels != .data$labels)
  
  if (!any(tidy$label_match)) {
    tidy <- tidy %>%
      select(
        -.data$coefno,
        -.data$coef_labels,
        -.data$label_match
      )
  } else {
    stop("There was a parsing error")
  }
  
  tidy <- add_variable(tidy, labels = "model", vals = class(model)[[1]])
  tidy <- add_variable(tidy, labels = "version", vals = "1.0")
  tidy <- add_variable(tidy, labels = "residual", vals = model$df.residual)
  
  if (length(summary(model)$sigma^2) > 0) {
    tidy <- add_variable(tidy, labels = "sigma2", vals = summary(model)$sigma^2)
  }
  
  if (!is.null(model$family$family)) {
    tidy <- add_variable(tidy, labels = "family", vals = model$family$family)
  }
  
  if (!is.null(model$family$link)) {
    tidy <- add_variable(tidy, labels = "link", vals = model$family$link)
  }
  
  offset <- model$call$offset
  if (!is.null(offset)) {
    tidy <- tidy %>%
      bind_rows(tibble(
        labels = "offset",
        vals = as.character(offset),
        type = "variable"
      ))
  }
  
  tidy
}

# Fit method ------------------------------------------------------
#' @export
tidypredict_fit.lm <- function(model) {
  parsedmodel <- parse_model(model)
  te_fit_lm(parsedmodel)
}

#' @export
tidypredict_interval.lm <- function(model, interval = 0.95) {
  parsedmodel <- parse_model(model)
  te_interval_lm(parsedmodel, interval)
}
