#' @export
parse_model.cubist <- function(model) {
  coefs <- model$coefficients
  splits <- model$splits
  splits$variable <- as.character(splits$variable)
  splits$dir <- as.character(splits$dir)

  committees2 <- map(
    unique(coefs$committee),
    ~ {
      comm <- .x
      rules <- map(
        coefs$rule[coefs$committee == comm],
        ~ {
          cc <- coefs[coefs$rule == .x & coefs$committee == comm, ]
          cs <- splits[splits$rule == .x & splits$committee == comm, ]
          tcs <- transpose(cs)
          mcs <- map(
            tcs,
            ~ list(
              type = "conditional",
              col = .x$variable,
              val = .x$value,
              op = ifelse(.x$dir == ">", "more-equal", "less")
            )
          )
          cc_names <- names(cc)
          f_coefs <- map(
            seq_along(cc_names),
            ~ {
              if (cc_names[.x] == "(Intercept)") {
                op <- "none"
                is_intercept <- 1
              } else {
                op <- "multiply"
                is_intercept <- 0
              }
              list(col = cc_names[.x], val = cc[, .x], op = op, is_intercept = is_intercept)
            }
          )

          f_na <- map_lgl(
            seq_along(cc_names),
            ~ !is.na(cc[, .x])
          )
          f_coefs <- f_coefs[f_na]
          f_coefs <- f_coefs[1:(length(f_coefs) - 2)]

          list(
            prediction = f_coefs,
            path = mcs
          )
        }
      )
    }
  )
  comm <- flatten(committees2)
  pm <- list(
    general = list(
      model = "cubist",
      type = "tree",
      version = 2,
      mode = "ifelse",
      divisor = model$committees
    ),
    trees = list(comm)
  )
  as_parsed_model(pm)
}

#' @export
tidypredict_fit.cubist <- function(model) {
  parsedmodel <- parse_model(model)
  build_fit_formula_rf(parsedmodel)
}
