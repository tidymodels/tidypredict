# tidypredict parses the tree structure C5.0 stores as text in `model$tree`
# rather than converting to a partykit object. Conversion via
# `partykit::as.party()` re-evaluates the fitting call to recover the training
# data, which is unavailable when the model is fit through the x/y interface
# (as parsnip does), so text parsing is the only path that works in general.

# Parse the `key="value"` attributes on a single C5.0 tree line.
parse_c50_attrs <- function(line) {
  matches <- gregexpr(
    '([a-z]+)="([^"]*(?:"[^"]*)*?)"(?=\\s|$)',
    line,
    perl = TRUE
  )
  starts <- matches[[1]]
  lengths <- attr(matches[[1]], "match.length")
  keys <- character()
  vals <- list()
  for (i in seq_along(starts)) {
    token <- substr(line, starts[i], starts[i] + lengths[i] - 1)
    eq <- regexpr("=", token, fixed = TRUE)
    key <- substr(token, 1, eq - 1)
    val <- substr(token, eq + 2, nchar(token) - 1)
    keys[i] <- key
    vals[[i]] <- val
  }
  # `elts` can appear multiple times (one group of levels per fork).
  elts <- unlist(vals[keys == "elts"])
  attrs <- vals[keys != "elts"]
  names(attrs) <- keys[keys != "elts"]
  attrs$elts <- elts
  attrs
}

# Parse `model$tree` into a nested list of split and leaf nodes.
parse_c50_tree <- function(model) {
  lines <- strsplit(model$tree, "\n")[[1]]
  lines <- lines[nzchar(lines)]
  lines <- lines[!grepl("^(id=|entries=)", lines)]

  pos <- 1L
  read_node <- function() {
    attrs <- parse_c50_attrs(lines[[pos]])
    pos <<- pos + 1L
    type <- attrs$type

    if (type == "0") {
      return(list(kind = "leaf", prediction = attrs$class))
    }

    forks <- as.integer(attrs$forks)
    kids <- lapply(seq_len(forks), function(i) read_node())

    if (type == "2") {
      # Continuous split. The three forks are, in order, the missing-value
      # branch (ignored, NAs are not handled), `<= cut`, and `> cut`.
      if (forks != 3) {
        cli::cli_abort("Unsupported C5.0 continuous split with {forks} forks.")
      }
      list(
        kind = "cont",
        col = attrs$att,
        val = as.numeric(attrs$cut),
        left = kids[[2]],
        right = kids[[3]]
      )
    } else if (type == "3") {
      # Categorical split. Each fork holds a group of factor levels (`elts`).
      groups <- lapply(attrs$elts, function(g) {
        vals <- strsplit(g, ",")[[1]]
        gsub('^"|"$', "", vals)
      })
      list(kind = "cat", col = attrs$att, groups = groups, kids = kids)
    } else {
      cli::cli_abort("Unsupported C5.0 node type {.val {type}}.")
    }
  }

  read_node()
}

# Flatten the nested tree into the binary `tree_info` structure consumed by
# the nested case_when generator. Multiway categorical splits are expanded
# into a chain of binary `%in%` splits.
c50_tree_info <- function(node) {
  acc <- new.env(parent = emptyenv())
  acc$rows <- list()
  acc$counter <- -1L

  new_id <- function() {
    acc$counter <- acc$counter + 1L
    acc$counter
  }

  add_row <- function(row) {
    acc$rows[[length(acc$rows) + 1L]] <- row
  }

  emit_cat <- function(col, groups, kids) {
    id <- new_id()
    left_id <- emit(kids[[1]])
    if (length(groups) == 2) {
      right_id <- emit(kids[[2]])
    } else {
      right_id <- emit_cat(col, groups[-1], kids[-1])
    }
    add_row(list(
      nodeID = id,
      leftChild = left_id,
      rightChild = right_id,
      splitvarName = col,
      terminal = FALSE,
      prediction = NA_character_,
      split = list(
        primary = list(
          col = col,
          vals = as.list(groups[[1]]),
          is_categorical = TRUE,
          needs_swap = FALSE
        )
      )
    ))
    id
  }

  emit <- function(node) {
    if (node$kind == "leaf") {
      id <- new_id()
      add_row(list(
        nodeID = id,
        leftChild = NA_integer_,
        rightChild = NA_integer_,
        splitvarName = NA_character_,
        terminal = TRUE,
        prediction = node$prediction,
        split = list(NULL)
      ))
      return(id)
    }

    if (node$kind == "cat") {
      return(emit_cat(node$col, node$groups, node$kids))
    }

    id <- new_id()
    left_id <- emit(node$left)
    right_id <- emit(node$right)
    add_row(list(
      nodeID = id,
      leftChild = left_id,
      rightChild = right_id,
      splitvarName = node$col,
      terminal = FALSE,
      prediction = NA_character_,
      split = list(
        primary = list(
          col = node$col,
          val = node$val,
          is_categorical = FALSE,
          needs_swap = FALSE
        )
      )
    ))
    id
  }

  emit(node)

  rows <- acc$rows
  n <- length(rows)
  list(
    nodeID = map_int(rows, ~ .x$nodeID),
    leftChild = map_int(rows, ~ .x$leftChild),
    rightChild = map_int(rows, ~ .x$rightChild),
    splitvarName = map_chr(rows, ~ .x$splitvarName),
    terminal = map_lgl(rows, ~ .x$terminal),
    prediction = map_chr(rows, ~ .x$prediction),
    node_splits = map(rows, ~ .x$split),
    majority_left = rep(NA, n),
    use_surrogates = FALSE
  )
}

c50_tree_info_full <- function(model) {
  if (isTRUE(model$rbm)) {
    cli::cli_abort(
      "{.pkg tidypredict} does not support rule-based C5.0 models ({.code rules = TRUE})."
    )
  }
  if (as.integer(model$trials[["Actual"]]) > 1) {
    cli::cli_abort(
      "{.pkg tidypredict} does not support boosted C5.0 models ({.code trials > 1})."
    )
  }
  c50_tree_info(parse_c50_tree(model))
}

# Predict ---------------------------------------

#' @export
tidypredict_fit.C5.0 <- function(model, ...) {
  generate_nested_case_when_tree(c50_tree_info_full(model))
}

# Parse model --------------------------------------

#' @export
parse_model.C5.0 <- function(model) {
  pm <- list()
  pm$general$model <- "C5.0"
  pm$general$type <- "tree"
  pm$general$version <- 3
  pm$tree_info <- c50_tree_info_full(model)
  as_parsed_model(pm)
}

# For {orbital}
#' Extract comprehensive tree info for C5.0 models
#'
#' Returns tree structure in format needed by nested case_when generator.
#' For use in orbital package.
#' @param model A C5.0 model object
#' @keywords internal
#' @export
.c50_tree_info_full <- function(model) {
  c50_tree_info_full(model)
}
