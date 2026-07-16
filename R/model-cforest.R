# cforest is a random forest of partykit party trees. Prediction (with the
# default `scale = TRUE`) is the average across trees of each tree's in-bag
# weighted terminal-node mean, so it can be expressed as the mean of the
# individual party-tree expressions.

cforest_check_regression <- function(model) {
  response_col <- model$fitted[["(response)"]]
  if (!is.numeric(response_col)) {
    cli::cli_abort(
      c(
        "Classification models are not supported for cforest.",
        i = "Only regression models can be converted to tidy formulas.",
        i = "Classification requires a voting mechanism that cannot be expressed as a single formula."
      )
    )
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
    function(tree_no) partykit_tree_info_full(partykit::gettree(model, tree_no))
  )
  as_parsed_model(pm)
}

# Fit formula -----------------------------------

#' @export
tidypredict_fit.cforest <- function(model, ...) {
  cforest_check_regression(model)

  n_trees <- length(model$nodes)
  tree_exprs <- map(
    seq_len(n_trees),
    function(tree_no) {
      tree_info <- partykit_tree_info_full(partykit::gettree(model, tree_no))
      generate_nested_case_when_tree(tree_info)
    }
  )

  res <- reduce_addition(tree_exprs)
  expr_division(res, n_trees)
}
