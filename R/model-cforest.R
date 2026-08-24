# cforest is a random forest of partykit party trees. Prediction (with the
# default `scale = TRUE`) is the average across trees of each tree's in-bag
# weighted terminal-node mean, so it can be expressed as the mean of the
# individual party-tree expressions.

# partykit 1.3-0 added a shim to its methods that warns when a method is called
# directly instead of through the generic. It identifies the caller with
# `as.name(as.list(sys.call(-1))[[1L]])`, which errors on a call whose first
# element is not a symbol. The generic's own frame is the method's caller, so
# writing `partykit::gettree(...)` makes that first element the `::` call and
# every `gettree()` on a cforest model fails with "'language' object cannot be
# coerced to type 'symbol'".
#
# Binding the generic to a local name calls it through a symbol instead. The
# name has to be `gettree`, because the shim then compares that symbol against
# the generic's own name and stays quiet only if they match.
#
# Only `gettree` is affected. The other partykit functions used here
# (`nodeids`, `nodeapply`, `as.party`, `id_node`, `is.terminal`, `kids_node`)
# were checked and are fine with the `::` prefix.
cforest_gettree <- function(model, tree_no) {
  gettree <- partykit::gettree
  gettree(model, tree_no)
}

cforest_check_regression <- function(model) {
  response_col <- model$fitted[["(response)"]]
  if (!is.numeric(response_col)) {
    abort_classification_unsupported("cforest")
  }
  invisible(model)
}

# Model parser -------------------------------------

#' @export
parse_model.cforest <- function(model) {
  cforest_check_regression(model)

  pm <- list()
  pm$general$model <- "cforest"
  pm$general$type <- "tree"
  pm$general$version <- 3
  n_trees <- length(model$nodes)
  pm$tree_info_list <- map(
    seq_len(n_trees),
    function(tree_no) partykit_tree_info_full(cforest_gettree(model, tree_no))
  )
  as_parsed_model(pm)
}

# Fit formula -----------------------------------

#' @export
tidypredict_fit.cforest <- function(model, ...) {
  cforest_check_regression(model)

  tidypredict_combine_trees(model, tidypredict_trees(model))
}

# Extractors --------------------------------------

#' @export
tidypredict_trees.cforest <- function(x, ...) {
  rlang::check_dots_empty()
  cforest_check_regression(x)

  map(
    seq_len(length(x$nodes)),
    function(tree_no) {
      tree_info <- partykit_tree_info_full(cforest_gettree(x, tree_no))
      generate_nested_case_when_tree(tree_info, missing = "na")
    }
  )
}

#' @export
tidypredict_n_trees.cforest <- function(x, ...) {
  rlang::check_dots_empty()

  length(x$nodes)
}

#' @export
tidypredict_combine_trees.cforest <- function(x, trees, ...) {
  rlang::check_dots_empty()
  check_trees_arg(trees)

  expr_mean(trees, length(trees))
}

build_tree_formula.pm_tree_cforest <- function(model) {
  expr_mean(map(
    model$tree_info_list,
    \(tree_info) generate_nested_case_when_tree(tree_info, missing = "na")
  ))
}
