#' @export
tidypredict_fit.earth <- function(model) {
  parsedmodel <- parse_model(model)
  te_earth_fit(parsedmodel)
}

#' @export
parse_model.earth <- function(model) {
  get_values <- function(model, dirs, rep_value = "term") {
    term_labels <- attr(model$terms, "term.labels")
    col_names <- colnames(model$dirs)
    if (rep_value == "term") {
      map_val <- map_chr
    } else {
      map_val <- map_dbl
    }
    fields <- map_val(
      term_labels,
      ~{
        sub_cols <- substr(col_names, 1, nchar(.x))
        sub_val <- substr(col_names, nchar(.x) + 1, nchar(col_names))
        col_matched <- .x == sub_cols
        col_notzero <- dirs != 0
        col_select <- col_matched + col_notzero == 2
        if (any(col_select)) {
          if (rep_value == "term") {
            val_select <- sub_val[col_select]
            if (val_select == "") val_select <- "{{:}}"
          } else {
            val_select <- dirs[col_select]
          }
        } else {
          val_select <- NA
        }
        val_select
      }
    )
    names(fields) <- term_labels
    fields
  }
  
  coefs <- model$coefficients
  if (!is.null(model$glm.coefficients)) coefs <- model$glm.coefficients
  
  parsed <- map(
    rownames(coefs),
    ~{
      coefficients <- rownames(coefs) == .x
      coefficients <- coefs[coefficients, ]
      names(coefficients) <- "estimate"
      
      cdir <- rownames(model$dirs) == .x
      cdir <- model$dirs[cdir, ]
      
      ccut <- rownames(model$cuts) == .x
      ccut <- model$cuts[ccut, ]
      
      fields <- get_values(model, cdir)
      names(fields) <- paste0("field_", seq_along(fields))
      
      cuts <- get_values(model, ccut, "")
      names(cuts) <- paste0("cuts_", seq_along(cuts))
      
      dirs <- get_values(model, cdir, "")
      names(dirs) <- paste0("dirs_", seq_along(dirs))
      
      labels <- .x
      names(labels) <- "labels"
      
      type <- "terms"
      names(type) <- "type"
      
      
      row_vec <- c(labels, coefficients, type, fields, dirs, cuts)
      row_list <- as.list(row_vec)
    }
  )
  
  parsed_tibble <- map_df(parsed, ~as.tibble(.x))
  
  term_labels <- attr(model$terms, "term.labels")
  names(term_labels) <- paste0("field_", seq_along(term_labels))
  
  new <- c("labels", 0, "variable")
  names(new) <- c("labels", "estimate", "type")
  
  new <- c(new, term_labels)
  
  all_names <- colnames(parsed_tibble)
  
  rest_names <- all_names[length(new) + 1:(length(all_names) - length(new)) ]
  
  rest <- rep(NA, length(rest_names))
  names(rest) <- rest_names
  
  new <- c(new, rest)
  
  mt <- rbind(
    parsed_tibble,
    as.tibble(as.list(new))
  )
  
  mt <- bind_rows(
    mt,
    tibble(
      labels = "model",
      type = "variable",
      vals = "earth"
    )
  )
  
  if (!is.null(model$glm.coefficients)) {
    fam <- model$glm.list[[1]]$family
    mt <- bind_rows(
      mt,
      tribble(
        ~labels, ~vals,
        "family", fam$family,
        "link", fam$link
      )
    )
  }
  
  mt
}



te_earth_fit <- function(parsedmodel) {
  
  model_fields <- parsedmodel[parsedmodel$labels == "labels", ]
  model_cols <- colnames(model_fields)
  model_fields <- model_fields[, substr(names(model_fields), 1, 6) == "field_"]
  model_list <- as.list(model_fields)

  pm <- parsedmodel[parsedmodel$type == "terms", ]
  pm <- pm[!is.na(pm$labels), ]
  pm_list <- transpose(pm)

  all_terms <- map(
    seq_along(pm_list),
    ~{
      term_formula(pm_list[.x], model_list)
    }
  )

  fit <- reduce(all_terms, function(x, y) expr(!!x + !!y))

  link <- parsedmodel$vals[parsedmodel$labels == "link"]

  if (length(link) > 0) {
    assigned <- 0

    if (link == "identity") {
      assigned <- 1
    }

    if (link == "logit") {
      assigned <- 1
      fit <- expr(1 - 1 / (1 + exp(!!fit)))
    }

    if (link == "log") {
      assigned <- 1
      fit <- expr(exp(!!fit))
    }

    if (assigned == 0) {
      stop("Combination of family and link are not supported")
    }
  }
  fit
}

term_formula <- function(x, model_list) {
  x <- x[[1]]

  est <- as.numeric(x$estimate)

  sel_field <- substr(names(x), 1, 6) == "field_"
  sel_cuts <- substr(names(x), 1, 5) == "cuts_"
  sel_dirs <- substr(names(x), 1, 5) == "dirs_"

  fields <- x[sel_field]
  cuts <- x[sel_cuts]
  dirs <- x[sel_dirs]

  if (x$labels == "(Intercept)") {
    expr((!!est))
  } else {
    term <- map(
      seq_along(fields),
      ~{
        if (!is.na(fields[.x])) {
          field <- model_list[names(model_list) == names(fields[.x])][[1]]
          field <- sym(field)

          v <- fields[.x][[1]]
          dir <- as.numeric(dirs[.x])
          cut <- as.numeric(cuts[.x])
          ret <- NULL
          if (dir == 2) {
            ret <- expr(!!field)
          }
          if (v != "{{:}}") {
            ret <- expr(ifelse(!!field == !!v, 1, 0))
          }
          if (dir == 1) {
            ret <- expr(ifelse(!!field > !!cut, (!!field - !!cut), 0))
          }
          if (dir == -1) {
            ret <- expr(ifelse(!!field < !!cut, (!!cut - !!field), 0))
          }
          ret
        }
      }
    )
    not_nulls <- map_lgl(map(term, class), ~.x != "NULL")
    term <- reduce(term[not_nulls], function(x, y) expr(!!x * !!y))
    expr((!!as.numeric(x$estimate) * !!term))
  }
}
