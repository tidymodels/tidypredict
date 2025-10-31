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
          if (!is.null(model$splits)) {
            cs <- splits[splits$rule == .x & splits$committee == comm, ]
            tcs <- transpose(cs)
            mcs <- map(
              tcs,
              ~ list(
                type = "conditional",
                col = .x$variable,
                val = .x$value,
                op = ifelse(.x$dir == ">", "more", "less-equal")
              )
            )
          } else {
            mcs <- list(list(type = "all"))
          }
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
              list(
                col = cc_names[.x],
                val = cc[, .x],
                op = op,
                is_intercept = is_intercept
              )
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
  comm <- purrr::list_flatten(committees2)
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
  rules <- get_rf_case_tree(1, parsedmodel)
  paths <- lapply(parsedmodel$trees[[1]], function(x) path_formulas(x$path))

  n_committees <- model$committees

  if (n_committees == 1) {
    ommittee_id <- rep(1, length(rules))
  } else {
    model_print <- utils::capture.output(print(model))
    model_print <- model_print[grep(
      "Number of rules per committee",
      model_print
    )]
    model_print <- regmatches(
      model_print,
      m = gregexpr("[0-9]+", model_print)
    )[[
      1
    ]]
    ommittee_id <- as.integer(model_print)
    ommittee_id <- rep(seq_along(ommittee_id), times = ommittee_id)
  }

  committees <- purrr::map2(
    split(rules, ommittee_id),
    split(paths, ommittee_id),
    make_committee
  )

  out <- adder(committees)
  if (n_committees > 1) {
    # Average the committes
    out <- expr(!!out / !!n_committees)
  }

  out
}

make_committee <- function(rules, paths) {
  # cubist averages out rules if multiple apply
  paths <- lapply(paths, function(x) x %||% TRUE)
  paths <- adder(paths)
  rules <- adder(rules)
  if (identical(paths, TRUE)) {
    res <- rules
  } else {
    res <- expr(!!rules / !!paths)
  }
  res
}
