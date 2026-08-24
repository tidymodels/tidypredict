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

# The levels of each discrete predictor, taken from the `model$names` text where
# every attribute is declared on a line of the form `name: level, level, ...`
# (continuous attributes are declared as `name: continuous.`).
#
# A declaration can open with a marker in square brackets, `[ordered]` for an
# ordered predictor being the one that occurs in practice. It is not part of the
# first level and has to come off before the list is split.
c50_attr_levels <- function(model) {
  lines <- strsplit(model$names %||% "", "\n")[[1]]
  lines <- lines[!grepl("^\\s*\\|", lines)]
  lines <- lines[grepl(":", lines, fixed = TRUE)]

  res <- list()
  for (line in lines) {
    line <- sub("\\.$", "", trimws(line))
    pos <- regexpr(":", line, fixed = TRUE)
    name <- gsub("\\\\", "", trimws(substr(line, 1, pos - 1)))
    decl <- trimws(substr(line, pos + 1, nchar(line)))
    decl <- sub("^\\[[a-z]+\\]\\s*", "", decl)
    vals <- strsplit(decl, ",")[[1]]
    res[[name]] <- gsub("\\\\", "", trimws(vals))
  }
  res
}

# Fill in the confidence each leaf votes with when boosting trials are combined.
#
# `PredictTreeClassify()` scores the leaf's class as
# `(freq + prior) / (n_leaf + 1)`, where `prior` is the class proportion at the
# root of that trial's own tree. That is the same quantity C5.0 reports as the
# class probability, and it is not the Laplace ratio `(freq + 1) / (n_leaf + 2)`
# used before. It needs the root, so it cannot be worked out while descending
# the tree.
c50_set_leaf_predictions <- function(node, prior, levels) {
  if (node$kind == "leaf") {
    if (is.null(node$freq)) {
      return(node)
    }
    i <- match(node$prediction, levels)
    node$confidence <- (node$freq[[i]] + prior[[i]]) / (sum(node$freq) + 1)
    return(node)
  }

  if (node$kind == "cont") {
    node$left <- c50_set_leaf_predictions(node$left, prior, levels)
    node$right <- c50_set_leaf_predictions(node$right, prior, levels)
  } else {
    node$kids <- lapply(
      node$kids,
      c50_set_leaf_predictions,
      prior = prior,
      levels = levels
    )
  }
  node
}

# The class proportions recorded on a node, or an even split when it records
# no frequencies.
c50_node_prior <- function(attrs, levels) {
  if (is.null(attrs$freq)) {
    return(rep(1 / length(levels), length(levels)))
  }
  freq <- as.numeric(strsplit(attrs$freq, ",")[[1]])
  freq / sum(freq)
}

# Parse `model$tree` into a list of nested trees (one per boosting trial). A
# non-boosted model has a single tree; a boosted model (`trials > 1`) stores its
# trials concatenated, with the count in the `entries=` header line.
parse_c50_trees <- function(model) {
  if (!nzchar(model$tree %||% "")) {
    # `C5.0()` leaves the tree empty when fitting failed. A predictor name or
    # level containing `,` or `:` is one way to get there: those separate the
    # fields of the model text and C5.0 does not escape them.
    cli::cli_abort(c(
      "The model records no tree.",
      i = "{.fn C50::C5.0} writes one only when fitting succeeded.",
      i = "A predictor name or level containing {.val ,} or {.val :} is one cause."
    ))
  }

  lines <- strsplit(model$tree, "\n")[[1]]
  lines <- lines[nzchar(lines)]

  entries_line <- lines[grepl("^entries=", lines)]
  n_trees <- if (length(entries_line) > 0) {
    as.integer(parse_c50_attrs(entries_line[[1]])$entries)
  } else {
    1L
  }

  lines <- lines[!grepl("^(id=|entries=|costs=)", lines)]
  levels <- model$levels
  attr_levels <- c50_attr_levels(model)

  pos <- 1L
  read_node <- function() {
    attrs <- parse_c50_attrs(lines[[pos]])
    pos <<- pos + 1L
    type <- attrs$type

    freq <- if (!is.null(attrs$freq)) {
      as.numeric(strsplit(attrs$freq, ",")[[1]])
    } else {
      NULL
    }
    # `T->Cases` in the C sources. C5.0 omits `freq` when the node holds no
    # training cases, which is how the unused missing-value branch is written.
    cases <- if (is.null(freq)) 0 else sum(freq)

    if (type == "0") {
      # Filled in by `c50_set_leaf_predictions()`, which needs the root.
      return(list(
        kind = "leaf",
        prediction = attrs$class,
        confidence = NA_real_,
        freq = freq,
        cases = cases
      ))
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
        val = f32_split_boundary(as.numeric(attrs$cut), "upper"),
        left = kids[[2]],
        right = kids[[3]],
        freq = freq,
        cases = cases
      )
    } else if (type == "1") {
      # Discrete split with one fork per level of the attribute. The first fork
      # is the missing-value branch (ignored, NAs are not handled) and the rest
      # follow the order the levels are declared in.
      groups <- attr_levels[[attrs$att]]
      if (is.null(groups)) {
        # `model$names` separates one attribute from the next with `:` and one
        # level from the next with `,`, and C5.0 does not escape either when it
        # writes them, so a name containing one cannot be read back.
        cli::cli_abort(c(
          "Cannot read the levels of {.field {attrs$att}} from the model.",
          i = "A predictor name or level containing {.val ,} or {.val :} is not supported."
        ))
      }
      if (length(groups) != forks - 1) {
        cli::cli_abort(
          "Unsupported C5.0 discrete split on {.field {attrs$att}} with {forks} forks and {length(groups)} level{?s}."
        )
      }
      list(
        kind = "cat",
        col = attrs$att,
        groups = as.list(groups),
        kids = kids[-1],
        freq = freq,
        cases = cases
      )
    } else if (type == "3") {
      # Categorical split. Each fork holds a group of factor levels (`elts`).
      groups <- lapply(attrs$elts, function(g) {
        vals <- strsplit(g, ",")[[1]]
        gsub('^"|"$', "", vals)
      })
      list(
        kind = "cat",
        col = attrs$att,
        groups = groups,
        kids = kids,
        freq = freq,
        cases = cases
      )
    } else {
      cli::cli_abort("Unsupported C5.0 node type {.val {type}}.")
    }
  }

  # Each trial's leaves are scored against the priors at its own root.
  roots <- vector("list", n_trees)
  trees <- lapply(seq_len(n_trees), function(i) {
    roots[[i]] <<- parse_c50_attrs(lines[[pos]])
    read_node()
  })

  lapply(seq_len(n_trees), function(i) {
    tree <- c50_set_leaf_predictions(
      trees[[i]],
      c50_node_prior(roots[[i]], levels),
      levels
    )
    # `Pruned[t]->Leaf` in the C sources: the class `SelectClassGen()` starts
    # from, and so gives any tie to, when this trial classifies a case.
    tree$root_class <- roots[[i]]$class
    tree
  })
}

# Parse a single (non-boosted) C5.0 tree into a nested list of nodes.
parse_c50_tree <- function(model) {
  parse_c50_trees(model)[[1]]
}

# Flatten the nested tree into the binary `tree_info` structure consumed by
# the nested case_when generator. Multiway categorical splits are expanded
# into a chain of binary `%in%` splits.
c50_tree_info <- function(node, classes = NULL, default = NULL) {
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

  # The class distribution of a set of sibling nodes, `Nil` when none of them
  # records one.
  sum_freq <- function(nodes) {
    freqs <- Filter(Negate(is.null), lapply(nodes, function(x) x$freq))
    if (length(freqs) == 0) {
      return(NULL)
    }
    Reduce(`+`, freqs)
  }

  sum_cases <- function(nodes) {
    sum(vapply(nodes, function(x) x$cases %||% 0, numeric(1)))
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
      confidence = NA_real_,
      freq = sum_freq(kids),
      cases_left = kids[[1]]$cases %||% 0,
      cases_right = sum_cases(kids[-1]),
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
        confidence = node$confidence %||% NA_real_,
        freq = node$freq,
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
      confidence = NA_real_,
      freq = node$freq,
      cases_left = node$left$cases %||% 0,
      cases_right = node$right$cases %||% 0,
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
    confidence = map_dbl(rows, ~ .x$confidence),
    leaf_freq = map(rows, ~ if (isTRUE(.x$terminal)) .x$freq else NULL),
    # The class distribution recorded on every node, and the training case
    # counts of its two branches. Needed to reproduce the weighted descent
    # C5.0 uses when a split value is missing.
    node_freq = map(rows, ~ .x$freq),
    cases_left = map_dbl(rows, ~ .x$cases_left %||% NA_real_),
    cases_right = map_dbl(rows, ~ .x$cases_right %||% NA_real_),
    node_splits = map(rows, ~ .x$split),
    # The outcome levels and the class C5.0 falls back on, which the weighted
    # descent needs to break ties the way `SelectClassGen()` does.
    classes = classes,
    default_class = default,
    majority_left = rep(NA, n),
    use_surrogates = FALSE
  )
}

# Missing values ---------------------------------
#
# `FindLeafGen()` in C5.0's `classify.c` does not send a case with a missing
# split value down one branch. It calls `PredictFollowAllBranches()`, which
# descends *every* branch of the node carrying a fraction of the case,
# `Fraction * Branch[v]->Cases / T->Cases`, and each leaf reached adds
# `Fraction * ClassDist[c] / Cases` to `ClassSum[c]`. `SelectClassGen()` then
# returns the class with the largest `ClassSum`, starting from the tree's own
# default class and replacing only on a strict `>`, so the default wins ties.
#
# `ClassSum[c]` is therefore the value of this recursion, which is what
# `c50_na_score()` writes out as an expression:
#
#   leaf   ->  ClassDist[c] / Cases
#   split  ->  missing: (wl * left + wr * right) / (wl + wr)
#             otherwise: left or right, as the comparison decides
#
# A branch with no training cases is skipped by `PredictFollowAllBranches()`,
# and a leaf with no cases reached directly scores against its parent instead
# (the `T = PT` line in `FindLeafGen()`), which `parent_freq` carries down.

# The share of a node's class distribution belonging to `class_index`, or `NULL`
# when the node holds no cases at all.
c50_leaf_share <- function(freq, class_index) {
  if (is.null(freq) || sum(freq) == 0) {
    return(NULL)
  }
  freq[[class_index]] / sum(freq)
}

# The share of the case that goes down the left branch: the branch's share of
# the node's training cases when the split value is missing, and the ordinary
# 1/0 of the comparison otherwise.
c50_na_weight <- function(idx, tree_info) {
  wl <- tree_info$cases_left[[idx]]
  wr <- tree_info$cases_right[[idx]]
  total <- wl + wr
  missing_weight <- if (is.na(total) || total == 0) 0 else wl / total

  primary <- tree_info$node_splits[[idx]]$primary
  expr(case_when(
    !!build_nested_split_missing(primary) ~ !!missing_weight,
    !!build_nested_split_condition(primary) ~ 1,
    .default = 0
  ))
}

c50_na_score <- function(node_id, tree_info, class_index, parent_freq = NULL) {
  idx <- which(tree_info$nodeID == node_id)

  if (tree_info$terminal[idx]) {
    share <- c50_leaf_share(tree_info$leaf_freq[[idx]], class_index)
    if (is.null(share)) {
      share <- c50_leaf_share(parent_freq, class_index) %||% 0
    }
    return(share)
  }

  freq <- tree_info$node_freq[[idx]]
  left <- c50_na_score(
    tree_info$leftChild[idx],
    tree_info,
    class_index,
    parent_freq = freq
  )
  right <- c50_na_score(
    tree_info$rightChild[idx],
    tree_info,
    class_index,
    parent_freq = freq
  )

  # Written as `w * left + (1 - w) * right` rather than as one `case_when()`
  # with the two subtrees under both the missing arm and the comparison arm.
  # The latter states each subtree twice, which doubles the expression at every
  # level of the tree.
  weight <- c50_na_weight(idx, tree_info)

  reduce_addition(list(
    expr_multiplication(weight, left),
    expr_multiplication(expr(1 - !!weight), right)
  ))
}

# `ClassSum[0]` in `FindLeafGen()`: the same weighted descent as
# `c50_na_score()`, but each leaf reached contributes `Fraction * Cases` rather
# than a class share. A leaf with no cases of its own is again scored against
# its parent, matching the `T = PT` line.
c50_na_cases <- function(node_id, tree_info, parent_cases = NULL) {
  idx <- which(tree_info$nodeID == node_id)

  if (tree_info$terminal[idx]) {
    freq <- tree_info$leaf_freq[[idx]]
    cases <- if (is.null(freq)) 0 else sum(freq)
    if (cases == 0) {
      cases <- parent_cases %||% 0
    }
    return(cases)
  }

  freq <- tree_info$node_freq[[idx]]
  cases <- if (is.null(freq)) NULL else sum(freq)
  left <- c50_na_cases(
    tree_info$leftChild[idx],
    tree_info,
    parent_cases = cases
  )
  right <- c50_na_cases(
    tree_info$rightChild[idx],
    tree_info,
    parent_cases = cases
  )

  weight <- c50_na_weight(idx, tree_info)

  reduce_addition(list(
    expr_multiplication(weight, left),
    expr_multiplication(expr(1 - !!weight), right)
  ))
}

# Class probabilities under a missing split value.
#
# `PredictTreeClassify()` in `classify.c` accumulates the weighted class shares
# in `ClassSum[c]` and the weighted leaf count in `ClassSum[0]`, then reports
#
#   ClassSum[c] <- (ClassSum[0] * ClassSum[c] + Prior[c]) / (ClassSum[0] + 1)
#
# with `Prior[c]` the class proportion at the root of the tree. With no missing
# value the case reaches a single leaf, so `ClassSum[0]` is that leaf's case
# count and the formula collapses to the `(freq + prior) / (n + 1)` the ordinary
# nested `case_when()` already returns. Only rows actually missing a split value
# need the weighted form, so they are the only ones routed to it.
c50_classprob_with_na_descent <- function(fit, tree_info) {
  class_index <- tree_info$prob_class_index
  prior <- tree_info$prob_prior
  cols <- unique(tree_info$splitvarName[!is.na(tree_info$splitvarName)])
  # A stump has nothing to descend, and a parsed model saved before this was
  # recorded cannot be given the weighted branch.
  if (
    length(cols) == 0 ||
      is.null(tree_info$node_freq) ||
      is.null(class_index) ||
      is.null(prior)
  ) {
    return(fit)
  }

  n <- c50_na_cases(0L, tree_info)
  share <- c50_na_score(0L, tree_info, class_index)
  prob <- expr((!!expr_multiplication(n, share) + !!prior) / (!!n + 1))

  any_missing <- reduce_or(map(cols, \(col) expr(is.na(!!rlang::sym(col)))))
  expr(case_when(
    !!any_missing ~ !!prob,
    .default = !!fit
  ))
}

# The expression for one class-probability tree, weighted descent included when
# the tree carries what that needs (C5.0) and plain otherwise (rpart).
classprob_tree_expr <- function(tree_info) {
  c50_classprob_with_na_descent(
    generate_nested_case_when_tree(tree_info),
    tree_info
  )
}

# Wrap the ordinary nested `case_when()` so that only rows actually missing a
# split value pay for the weighted descent. Rows with no missing split value
# reach exactly one leaf under either route, so the guard changes nothing for
# them and keeps the expression small.
c50_with_na_descent <- function(fit, tree_info) {
  classes <- tree_info$classes
  default <- tree_info$default_class
  cols <- unique(tree_info$splitvarName[!is.na(tree_info$splitvarName)])
  # A stump has nothing to descend, and a parsed model saved before this was
  # recorded cannot be given the weighted branch.
  if (
    length(cols) == 0 ||
      is.null(tree_info$node_freq) ||
      is.null(classes) ||
      is.null(default)
  ) {
    return(fit)
  }

  ordered <- c(default, setdiff(classes, default))
  scores <- lapply(
    ordered,
    function(class) c50_na_score(0L, tree_info, match(class, classes))
  )

  any_missing <- reduce_or(map(cols, \(col) expr(is.na(!!rlang::sym(col)))))
  expr(case_when(
    !!any_missing ~ !!build_argmax_case_when(scores, ordered),
    .default = !!fit
  ))
}

c50_check_supported <- function(model, call = rlang::caller_env()) {
  if (isTRUE(model$control$fuzzyThreshold) || c50_has_soft_threshold(model)) {
    # Fuzzy thresholds route cases near a split point partly down both branches,
    # which cannot be expressed as a hard `<= cut` comparison.
    cli::cli_abort(
      "{.pkg tidypredict} does not support C5.0 models with fuzzy thresholds ({.code fuzzyThreshold = TRUE}).",
      call = call
    )
  }
  if (!is.null(model$costMatrix)) {
    # A cost matrix changes how the final class is chosen from the votes, which
    # the generated argmax expression does not account for.
    cli::cli_abort(
      "{.pkg tidypredict} does not support C5.0 models fitted with a cost matrix ({.code costs}).",
      call = call
    )
  }
  invisible(model)
}

# Whether the tree was built with fuzzy thresholds.
#
# `model$control` is the direct record, but {baguette} runs its base fits
# through `butcher()`, which empties it. A soft threshold is also visible in
# the tree text itself: C5.0 writes the `low` and `high` bounds it interpolates
# between alongside the `cut` of every continuous split, and writes neither for
# a hard threshold.
c50_has_soft_threshold <- function(model) {
  is.character(model$tree) && any(grepl("\" low=\"", model$tree, fixed = TRUE))
}

c50_tree_info_full <- function(model) {
  c50_check_supported(model)
  if (as.integer(model$trials[["Actual"]]) > 1) {
    cli::cli_abort(
      "{.pkg tidypredict} does not support boosted C5.0 models ({.code trials > 1})."
    )
  }
  c50_tree_info(parse_c50_tree(model))
}

# The class C5.0 falls back on, and gives any tie to: `Pruned[0]->Leaf`, the
# class recorded at the root of the first tree.
c50_default_class <- function(model) {
  lines <- strsplit(model$tree, "\n")[[1]]
  lines <- lines[nzchar(lines)]
  lines <- lines[!grepl("^(id=|entries=|costs=)", lines)]
  parse_c50_attrs(lines[[1]])$class
}

# The class proportions of the training sample, taken from the frequencies
# C5.0 records on the root node of the first tree.
c50_priors <- function(model) {
  lines <- strsplit(model$tree, "\n")[[1]]
  lines <- lines[nzchar(lines)]
  lines <- lines[!grepl("^(id=|entries=|costs=)", lines)]
  freq <- as.numeric(strsplit(parse_c50_attrs(lines[[1]])$freq, ",")[[1]])
  freq / sum(freq)
}

# One tree_info per outcome level, where the node predictions are the class
# probabilities instead of the predicted class. C5.0 reports the probability of
# a class at a leaf as `(freq + prior) / (n_leaf + 1)`, where `freq` is the
# number of training cases of that class reaching the leaf and `prior` is the
# class proportion of the whole training sample.
c50_classprob_tree_info <- function(model, call = rlang::caller_env()) {
  if (isTRUE(model$rbm)) {
    cli::cli_abort(
      "Class probabilities are not supported for rule-based C5.0 models ({.code rules = TRUE}).",
      call = call
    )
  }
  c50_check_supported(model, call = call)

  classes <- model$levels
  priors <- c50_priors(model)
  tree_info <- c50_tree_info_full(model)

  probs <- map(tree_info$leaf_freq, function(freq) {
    if (is.null(freq)) {
      rep(0, length(classes))
    } else {
      (freq + priors) / (sum(freq) + 1)
    }
  })

  res <- map(seq_along(classes), function(i) {
    ti <- tree_info_with_predictions(tree_info, map_dbl(probs, ~ .x[[i]]))
    # Which class this tree scores, and that class's prior, so the weighted
    # descent used for a missing split value can rebuild the probability.
    ti$prob_class_index <- i
    ti$prob_prior <- priors[[i]]
    ti
  })
  names(res) <- classes
  res
}

# Build a nested case_when returning, at each leaf, the leaf confidence when the
# leaf predicts `class` and 0 otherwise. Summed across trials this gives the
# total confidence-weighted vote C5.0 assigns to `class`.
c50_class_vote <- function(tree_info, class) {
  generate_nested_case_when_tree(tree_info_with_predictions(
    tree_info,
    ifelse(
      tree_info$terminal & tree_info$prediction == class,
      tree_info$confidence,
      0
    )
  ))
}

# Combine boosted trials by confidence-weighted voting. C5.0 predicts the class
# with the greatest total vote, and `SelectClass` starts from the default class
# and replaces only on a strict `>`, so the default wins any tie it is part of.
# Checking it first with `>=` reproduces that, as the rules path also does.
c50_boosted_case_when <- function(tree_info_list, classes, default = NULL) {
  ordered <- if (is.null(default)) {
    classes
  } else {
    c(default, setdiff(classes, default))
  }

  votes <- lapply(ordered, function(class) {
    reduce_addition(lapply(tree_info_list, c50_class_vote, class = class))
  })

  build_argmax_case_when(votes, ordered)
}

# Boosting under a missing split value
#
# `PredictBoostClassify()` in C5.0's `classify.c` runs each trial in turn:
#
#   Best = PredictTreeClassify(Case, Pruned[t])
#   Vote[Best] += Confidence
#
# and finally takes `SelectClassGen()` over `Vote[c] / Total`. Dividing by
# `Total` is a positive constant and cannot move the argmax, so it is dropped.
#
# `PredictTreeClassify()` picks `Best` as the argmax of the *raw* accumulated
# `ClassSum[c]`, starting from that trial's own root class, and only then
# rewrites `ClassSum[c]` as `(W * ClassSum[c] + prior[c]) / (W + 1)` and takes
# `Confidence = ClassSum[Best]`. With no missing split value the case reaches a
# single leaf, so `Best` is that leaf's recorded class and `Confidence` is the
# leaf confidence the ordinary expression already votes with. A missing split
# value makes both the argmax and the confidence depend on the weighted descent,
# which is what this rebuilds.

# Whether each score is the first of `scores` to attain the maximum, which is
# what `SelectClassGen()` returns when its default class is put first: it starts
# from the default and replaces only on a strict `>`.
c50_argmax_indicators <- function(scores) {
  n <- length(scores)
  lapply(seq_len(n), function(j) {
    conds <- c(
      lapply(seq_len(j - 1L), \(i) expr(!!scores[[j]] > !!scores[[i]])),
      lapply(seq_len(n - j) + j, \(i) expr(!!scores[[j]] >= !!scores[[i]]))
    )
    combine_path_conditions(conds)
  })
}

# One trial's confidence-weighted vote for every class, named by class.
c50_trial_na_votes <- function(tree_info, classes) {
  root <- which(tree_info$nodeID == 0L)
  root_freq <- tree_info$node_freq[[root]]
  priors <- root_freq / sum(root_freq)

  ordered <- c(
    tree_info$default_class,
    setdiff(classes, tree_info$default_class)
  )
  index <- match(ordered, classes)

  scores <- lapply(index, function(i) c50_na_score(0L, tree_info, i))
  n <- c50_na_cases(0L, tree_info)
  wins <- c50_argmax_indicators(scores)

  votes <- lapply(seq_along(ordered), function(j) {
    confidence <- expr(
      (!!expr_multiplication(n, scores[[j]]) + !!priors[[index[[j]]]]) /
        (!!n + 1)
    )
    expr(dplyr::if_else(!!wins[[j]], !!confidence, 0))
  })
  names(votes) <- ordered
  votes
}

# Whether a trial carries everything the weighted descent needs. A model parsed
# before these fields were recorded, or one whose trials are all stumps, keeps
# the expression it had.
c50_boosted_na_ready <- function(tree_info_list) {
  all(vapply(
    tree_info_list,
    function(x) {
      root <- which(x$nodeID == 0L)
      !is.null(x$node_freq) &&
        !is.null(x$default_class) &&
        length(root) == 1 &&
        !is.null(x$node_freq[[root]])
    },
    logical(1)
  ))
}

c50_boosted_with_na_descent <- function(fit, tree_info_list, classes, default) {
  cols <- unique(unlist(lapply(
    tree_info_list,
    function(x) x$splitvarName[!is.na(x$splitvarName)]
  )))
  if (
    length(cols) == 0 ||
      is.null(classes) ||
      is.null(default) ||
      !c50_boosted_na_ready(tree_info_list)
  ) {
    return(fit)
  }

  trial_votes <- lapply(tree_info_list, c50_trial_na_votes, classes = classes)

  ordered <- c(default, setdiff(classes, default))
  votes <- lapply(ordered, function(class) {
    reduce_addition(lapply(trial_votes, function(x) x[[class]]))
  })

  any_missing <- reduce_or(map(cols, \(col) expr(is.na(!!rlang::sym(col)))))
  expr(case_when(
    !!any_missing ~ !!build_argmax_case_when(votes, ordered),
    .default = !!fit
  ))
}

# Rules -----------------------------------------

# A rule-based C5.0 model (`rules = TRUE`, the engine behind parsnip's
# `C5_rules()`) stores an ordered list of conjunctive rules as text in
# `model$rules` rather than a tree in `model$tree`. Each rule votes for its
# right-hand-side class with its Laplace confidence `(ok + 1) / (cover + 2)`
# when all of its conditions hold. Prediction sums those votes per class and
# returns the class with the greatest total, falling back to the `default`
# class (which also wins ties), matching C5.0's `RuleClassify`/`SelectClass`.

# Turn a single parsed condition line into a serializable description. Three
# condition kinds occur in rule sets: type 1 is discrete equality, type 2 a
# continuous threshold (`<` means `<= cut`, `>` means `> cut`), and type 3 a
# discrete subset membership.
c50_rule_condition <- function(attrs) {
  type <- attrs$type
  if (type == "1") {
    list(col = attrs$att, op = "eq", val = attrs$val)
  } else if (type == "2") {
    list(
      col = attrs$att,
      op = if (attrs$result == "<") "le" else "gt",
      val = f32_split_boundary(as.numeric(attrs$cut), "upper")
    )
  } else if (type == "3") {
    vals <- gsub('^"|"$', "", strsplit(attrs$elts, '","')[[1]])
    list(col = attrs$att, op = "in", vals = as.list(vals))
  } else {
    cli::cli_abort("Unsupported C5.0 rule condition type {.val {type}}.")
  }
}

# Parse `model$rules` into a serializable list of rules plus the class levels
# and default class.
parse_c50_rules <- function(model) {
  lines <- strsplit(model$rules, "\n")[[1]]
  lines <- lines[nzchar(lines)]

  entries_line <- lines[grepl("^entries=", lines)]
  n_trees <- if (length(entries_line) > 0) {
    as.integer(parse_c50_attrs(entries_line[[1]])$entries)
  } else {
    1L
  }
  if (n_trees > 1) {
    cli::cli_abort(
      "{.pkg tidypredict} does not support boosted rule-based C5.0 models ({.code rules = TRUE} with {.code trials > 1})."
    )
  }

  lines <- lines[!grepl("^(id=|entries=|costs=)", lines)]

  header <- parse_c50_attrs(lines[[1]])
  default <- header$default
  lines <- lines[-1]

  rules <- list()
  i <- 1L
  while (i <= length(lines)) {
    attrs <- parse_c50_attrs(lines[[i]])
    nconds <- as.integer(attrs$conds)
    conds <- lapply(seq_len(nconds), function(k) {
      c50_rule_condition(parse_c50_attrs(lines[[i + k]]))
    })
    i <- i + nconds + 1L
    # Round so the value survives a YAML save/reload round-trip; the extra
    # precision never changes the vote argmax.
    confidence <- round(
      (as.numeric(attrs$ok) + 1) / (as.numeric(attrs$cover) + 2),
      7
    )
    rules[[length(rules) + 1L]] <- list(
      class = attrs$class,
      confidence = confidence,
      conditions = conds
    )
  }

  list(rules = rules, classes = model$levels, default = default)
}

# Missing values
#
# `Matches()` in C5.0's `classify.c` fires a rule only when every condition
# `Satisfies()` it, and `Satisfies()` compares `FindOutcome()` against the
# condition's own `TestValue`. `FindOutcome()` returns -1 when the tested
# attribute is unknown, and no `TestValue` is -1, so a rule that tests a
# missing attribute never fires. (The rule-tree route, `MarkActive()`, reaches
# the same place: it only descends `Branch[v]` for `v > 0`.) There is no
# weighted descent here as there is for a tree: a skipped rule simply
# contributes nothing to `ClassSum`, and `PredictRuleClassify()` returns the
# rule set's own default class when no rule fires at all, which the argmax
# cascade below already does because every vote is then 0.
#
# So the correct behaviour is exactly the vote sum with a missing-value
# condition treated as false. R makes `NA <= cut` and `f == "a"` return `NA`
# rather than `FALSE`, which poisoned the sum and made the whole cascade fall
# through, so every column a rule tests is guarded with `!is.na()`.
c50_rule_cols <- function(rule) {
  unique(vapply(rule$conditions, function(x) x$col, character(1)))
}

c50_condition_expr <- function(cond) {
  col <- sym(cond$col)
  switch(
    cond$op,
    eq = expr(!!col == !!cond$val),
    le = expr(!!col <= !!cond$val),
    gt = expr(!!col > !!cond$val),
    "in" = expr(!!col %in% !!unlist(cond$vals))
  )
}

# Total confidence-weighted vote for one class: the sum, over every rule that
# predicts `class`, of the rule's confidence when its conditions hold and 0
# otherwise. A class with no rules contributes a constant 0.
c50_class_vote_rules <- function(rules_obj, class) {
  matching <- Filter(function(r) r$class == class, rules_obj$rules)
  if (length(matching) == 0) {
    return(0)
  }
  terms <- lapply(matching, function(r) {
    conds <- c(
      lapply(c50_rule_cols(r), \(col) expr(!is.na(!!sym(col)))),
      lapply(r$conditions, c50_condition_expr)
    )
    condition <- combine_path_conditions(conds)
    expr(dplyr::if_else(!!condition, !!r$confidence, 0))
  })
  reduce_addition(terms)
}

# Build the argmax cascade. Checking the default class first with `>=` makes it
# win ties (and the no-rule-fires case, where every vote is 0), reproducing
# `SelectClass`. Remaining classes follow in level order; the earliest class
# whose vote is at least every later class's wins.
c50_rules_case_when <- function(rules_obj) {
  classes <- rules_obj$classes
  default <- rules_obj$default

  # No rules: every case falls to the default class.
  if (length(rules_obj$rules) == 0) {
    return(default)
  }

  ordered <- c(default, setdiff(classes, default))

  votes <- lapply(ordered, function(cl) c50_class_vote_rules(rules_obj, cl))

  build_argmax_case_when(votes, ordered)
}

# Predict ---------------------------------------

#' @export
tidypredict_fit.C5.0 <- function(model, ...) {
  c50_check_supported(model)
  if (isTRUE(model$rbm)) {
    return(c50_rules_case_when(parse_c50_rules(model)))
  }
  trees <- parse_c50_trees(model)
  if (length(trees) == 1) {
    tree_info <- c50_tree_info(
      trees[[1]],
      model$levels,
      c50_default_class(model)
    )
    return(c50_with_na_descent(
      generate_nested_case_when_tree(tree_info),
      tree_info
    ))
  }
  tree_info_list <- c50_boosted_tree_info(trees, model$levels)
  classes <- model$levels
  default <- c50_default_class(model)
  c50_boosted_with_na_descent(
    c50_boosted_case_when(tree_info_list, classes, default),
    tree_info_list,
    classes,
    default
  )
}

# One `tree_info` per trial, each carrying the outcome levels and its own root
# class, which the weighted descent needs to break ties as `SelectClassGen()`
# does within that trial.
c50_boosted_tree_info <- function(trees, classes) {
  lapply(trees, function(tree) c50_tree_info(tree, classes, tree$root_class))
}

# Test ---------------------------------------------

# `C5.0()` keeps no copy of the training data, so `df` has no useful default.
#' @export
tidypredict_test.C5.0 <- function(
  model,
  df,
  threshold = 0.000000000001,
  include_intervals = FALSE,
  max_rows = NULL,
  xg_df = NULL
) {
  df <- maybe_head(df, max_rows)

  # C5.0 only fits classification models
  base <- predict(model, df, type = "class")

  te <- tidypredict_to_column(
    df,
    model,
    add_interval = FALSE,
    vars = c("fit_te", "upr_te", "lwr_te")
  )

  test_results_class(base, te$fit_te, model$call)
}

# Parse model --------------------------------------

#' @export
parse_model.C5.0 <- function(model) {
  c50_check_supported(model)
  pm <- list()
  pm$general$model <- "C5.0"
  pm$general$type <- "tree"
  pm$general$version <- 3
  if (isTRUE(model$rbm)) {
    pm$rules_info <- parse_c50_rules(model)
    return(as_parsed_model(pm))
  }
  trees <- parse_c50_trees(model)
  if (length(trees) == 1) {
    pm$tree_info <- c50_tree_info(
      trees[[1]],
      model$levels,
      c50_default_class(model)
    )
  } else {
    pm$tree_info_list <- c50_boosted_tree_info(trees, model$levels)
    pm$classes <- model$levels
    pm$default <- c50_default_class(model)
  }
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

# C5.0 has three shapes: a rule set, a boosted sequence of trees, and a single
# tree.
build_tree_formula.pm_tree_C5.0 <- function(model) {
  if (!is.null(model$rules_info)) {
    return(c50_rules_case_when(model$rules_info))
  }
  if (!is.null(model$tree_info_list)) {
    return(c50_boosted_with_na_descent(
      c50_boosted_case_when(
        model$tree_info_list,
        model$classes,
        model$default
      ),
      model$tree_info_list,
      model$classes,
      model$default
    ))
  }
  c50_with_na_descent(build_tree_formula_single(model), model$tree_info)
}

# Output metadata ---------------------------------

# C5.0 is classification only, and the fit votes the trees into a single class
# label rather than a probability.
#' @export
tidypredict_output_type.C5.0 <- function(x, ...) {
  rlang::check_dots_empty()
  "class"
}

#' @export
tidypredict_outcome_levels.C5.0 <- function(x, ...) {
  rlang::check_dots_empty()
  x$levels
}

#' @export
tidypredict_normalized.C5.0 <- function(x, ...) {
  rlang::check_dots_empty()
  NA
}
