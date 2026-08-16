#' @export
parse_model.cubist <- function(model) {
  coefs <- model$coefficients
  rules_text <- cubist_rules(model)

  # Pre-split data by committee and rule to avoid O(n) scans in nested loops
  coefs_by_comm_rule <- split(coefs, list(coefs$committee, coefs$rule))

  committee_levels <- unique(coefs$committee)

  committees2 <- map(
    committee_levels,
    ~ {
      comm <- .x
      rules <- map(
        coefs$rule[coefs$committee == comm],
        ~ {
          key <- paste(comm, .x, sep = ".")
          cc <- coefs_by_comm_rule[[key]]
          # `committee` and `rule` are character, so they index the parsed
          # rules only once converted back to positions.
          rule <- rules_text[[as.integer(comm)]][[as.integer(.x)]]
          mcs <- rule$path
          if (length(mcs) == 0) {
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
            path = mcs,
            limits = rule$limits
          )
        }
      )
    }
  )
  comm <- purrr::list_flatten(committees2)

  # Which committee each flattened rule belongs to, taken from
  # `model$coefficients` in the same order `comm` was built in. `print.Cubist`
  # truncates its "Number of rules per committee" line at 20 committees, so it
  # cannot be scraped for this.
  ommittee_id <- rep(
    seq_along(committee_levels),
    times = vapply(
      committee_levels,
      function(x) sum(coefs$committee == x),
      integer(1)
    )
  )

  pm <- list(
    general = list(
      model = "cubist",
      type = "tree",
      version = 3,
      mode = "ifelse",
      n_committees = model$committees,
      ommittee_id = ommittee_id,
      means = cubist_attribute_means(model)
    ),
    trees = list(comm)
  )
  as_parsed_model(pm)
}

# Every rule of every committee, as a list of committees of rules, each rule a
# list of its `path` and its `limits`.
#
# These are read from the model text rather than from `model$splits`, because
# `Cubist` only records numeric (`type="2"`) and subset (`type="3"`) conditions
# there. An equality condition on a categorical predictor (`type="1"`) is
# missing from `model$splits` altogether, which would widen the rule to every
# row.
cubist_rules <- function(model) {
  lines <- strsplit(model$model, "\n")[[1]]
  extrap <- as.numeric(cubist_field(lines[[2]], "extrap"))

  committees <- list()
  rules <- list()
  i <- 1

  while (i <= length(lines)) {
    line <- lines[[i]]
    if (grepl('^rules="', line)) {
      if (length(rules) > 0) {
        committees[[length(committees) + 1]] <- rules
      }
      rules <- list()
    } else if (grepl('^conds="', line)) {
      n <- as.integer(cubist_field(line, "conds"))
      conds <- lapply(seq_len(n), function(k) cubist_condition(lines[[i + k]]))
      rules[[length(rules) + 1]] <- list(
        path = conds,
        limits = cubist_rule_limits(line, extrap)
      )
      i <- i + n
    }
    i <- i + 1
  }
  if (length(rules) > 0) {
    committees[[length(committees) + 1]] <- rules
  }
  committees
}

# The range a rule's prediction is allowed to take.
#
# `Cubist` holds each rule to the span of the training outcomes it covers,
# widened at both ends by `extrap` times that span. The widened end never
# crosses zero: a rule that only ever saw non-negative outcomes cannot be
# extrapolated into negative ones, and the other way around.
cubist_rule_limits <- function(line, extrap) {
  lo <- as.numeric(cubist_field(line, "loval"))
  hi <- as.numeric(cubist_field(line, "hival"))
  span <- extrap * (hi - lo)

  lower <- lo - span
  if (lo >= 0) {
    lower <- max(lower, 0)
  }
  upper <- hi + span
  if (hi <= 0) {
    upper <- min(upper, 0)
  }
  c(lower, upper)
}

# The value of a `name="value"` field, or `NA` when the line has no such field.
cubist_field <- function(line, name) {
  match <- regmatches(line, regexec(paste0(name, '="([^"]*)"'), line))[[1]]
  if (length(match) < 2) {
    return(NA_character_)
  }
  match[[2]]
}

# Turn one condition line of the model text into a path element.
cubist_condition <- function(line) {
  type <- cubist_field(line, "type")
  col <- cubist_field(line, "att")

  if (type == "2") {
    return(list(
      type = "conditional",
      col = col,
      # Cubist compares as a 32-bit float, so a value that rounds
      # to the threshold belongs to the `<=` side.
      val = f32_split_boundary(as.numeric(cubist_field(line, "cut")), "upper"),
      op = if (cubist_field(line, "result") == ">") "more" else "less-equal"
    ))
  }

  if (type == "1") {
    vals <- list(cubist_field(line, "val"))
  } else {
    # A subset condition writes one quoted level per member, so a level
    # containing a comma stays in one piece.
    elts <- sub('^.*elts=', "", line)
    vals <- as.list(regmatches(elts, gregexpr('"[^"]*"', elts))[[1]])
    vals <- lapply(vals, \(x) gsub('^"|"$', "", x))
  }

  list(type = "set", col = col, vals = vals, op = "in")
}

#' @export
tidypredict_fit.cubist <- function(model) {
  parsedmodel <- parse_model(model)
  tidypredict_fit_cubist(parsedmodel)
}

# The per-predictor training means `Cubist` writes into its model text.
#
# `Cubist` substitutes these for a missing value, so they are what
# `predict()` uses. They are stored at the model's own precision rather than
# recomputed from the training data, because the rounded value is the one the
# C code reads back and predicts from.
cubist_attribute_means <- function(model) {
  lines <- strsplit(model$model, "\n")[[1]]
  matches <- regmatches(
    lines,
    regexec('^att="([^"]+)" mean="([^"]+)"', lines)
  )
  matches <- Filter(\(x) length(x) == 3 && x[[2]] != "outcome", matches)

  means <- lapply(matches, \(x) as.numeric(x[[3]]))
  names(means) <- vapply(matches, \(x) x[[2]], character(1))
  means
}

# Replace every mention of a predictor with its training mean where the value
# is missing. `Cubist` does this for the rule conditions as well as the linear
# models, so the substitution is applied to the whole expression rather than to
# the coefficients alone.
substitute_missing_means <- function(x, means) {
  if (is.symbol(x)) {
    mean <- means[[as.character(x)]]
    if (is.null(mean)) {
      return(x)
    }
    return(expr(ifelse(is.na(!!x), !!mean, !!x)))
  }
  if (is.call(x)) {
    # Position 1 holds the function being called, not a column.
    for (i in seq_along(x)[-1]) {
      if (!identical(x[[i]], quote(expr = ))) {
        x[[i]] <- substitute_missing_means(x[[i]], means)
      }
    }
  }
  x
}

tidypredict_fit_cubist <- function(parsedmodel) {
  rules <- generate_tree_nodes(parsedmodel$trees[[1]], parsedmodel$general$mode)
  paths <- lapply(parsedmodel$trees[[1]], function(x) path_formulas(x$path))

  n_committees <- parsedmodel$general$n_committees
  ommittee_id <- parsedmodel$general$ommittee_id

  committees <- purrr::map2(
    split(rules, ommittee_id),
    split(paths, ommittee_id),
    make_committee
  )

  out <- reduce_addition(committees)
  if (n_committees > 1) {
    # Average the committes
    out <- expr_division(out, n_committees)
  }

  means <- parsedmodel$general$means
  if (length(means) > 0) {
    out <- substitute_missing_means(out, means)
  }

  out
}

make_committee <- function(rules, paths) {
  # cubist averages out rules if multiple apply
  paths <- lapply(paths, function(x) x %||% TRUE)
  paths <- reduce_addition(paths)
  rules <- reduce_addition(rules)
  if (identical(paths, TRUE)) {
    res <- rules
  } else {
    res <- expr_division(rules, paths)
  }
  res
}

build_tree_formula.pm_tree_cubist <- function(model) {
  tidypredict_fit_cubist(model)
}
