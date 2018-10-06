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
