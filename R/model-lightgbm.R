# Model parser -------------------------------------

#' @export
parse_model.lgb.Booster <- function(model) {
  pm <- list()
  pm$general$model <- "lgb.Booster"
  pm$general$type <- "lgb"
  pm$general$version <- 1

  # Extract params (objective, etc.)
  pm$general$params <- model$params

  # Extract feature names from JSON dump
  model_json <- jsonlite::fromJSON(model$dump_model())
  pm$general$feature_names <- model_json$feature_names
  pm$general$nfeatures <- length(model_json$feature_names)

  # Extract number of iterations
  pm$general$niter <- model$current_iter()

  # Extract trees
  pm$trees <- get_lgb_trees(model)

  as_parsed_model(pm)
}

get_lgb_trees <- function(model) {
  trees_df <- lightgbm::lgb.model.dt.tree(model)
  trees_df <- as.data.frame(trees_df)

  # Check for unsupported categorical splits
  decision_types <- unique(trees_df$decision_type[
    !is.na(trees_df$decision_type)
  ])
  unsupported <- setdiff(decision_types, "<=")
  if (length(unsupported) > 0) {
    cli::cli_abort(
      c(
        "Unsupported decision type{?s} found: {.val {unsupported}}.",
        "i" = "Only {.val <=} (numerical splits) are currently supported.",
        "i" = "Categorical splits ({.val ==}) are not yet implemented."
      )
    )
  }

  # Split by tree_index
  trees_split <- split(trees_df, trees_df$tree_index)

  # Process each tree
  map(trees_split, get_lgb_tree)
}

get_lgb_children_map <- function(tree_df) {
  # For each split_index, find its children (ordered by row index)
  split_indices <- tree_df$split_index[!is.na(tree_df$split_index)]

  children_map <- lapply(split_indices, function(si) {
    # Children are rows where node_parent==si OR leaf_parent==si
    child_rows <- which(tree_df$node_parent == si | tree_df$leaf_parent == si)
    # Sort by row index: first is LEFT, second is RIGHT
    child_rows[order(child_rows)]
  })
  names(children_map) <- as.character(split_indices)
  children_map
}

get_lgb_tree <- function(tree_df) {
  # Build children map for direction detection
  children_map <- get_lgb_children_map(tree_df)

  # Find leaf rows
  leaf_rows <- which(!is.na(tree_df$leaf_index))

  # For each leaf, trace path to root
  map(leaf_rows, function(leaf_row) {
    list(
      prediction = tree_df$leaf_value[[leaf_row]],
      path = get_lgb_path(leaf_row, tree_df, children_map)
    )
  })
}

get_lgb_path <- function(leaf_row, tree_df, children_map) {
  path <- list()
  current_row <- leaf_row
  current_parent_split <- tree_df$leaf_parent[[leaf_row]]

  while (!is.na(current_parent_split)) {
    # Find the parent's row (split_index should be unique within a tree)
    parent_row <- which(tree_df$split_index == current_parent_split)[[1]]

    # Determine direction: is current_row the LEFT or RIGHT child?
    children <- children_map[[as.character(current_parent_split)]]
    is_left_child <- (current_row == children[1])

    # Build condition
    default_left <- tree_df$default_left[[parent_row]] == "TRUE"

    if (is_left_child) {
      # Went left: condition (feature <= threshold) was TRUE
      op <- "less-equal"
      # Missing goes with us if default_left is TRUE
      missing_with_us <- default_left
    } else {
      # Went right: condition (feature <= threshold) was FALSE
      op <- "more"
      # Missing goes with us if default_left is FALSE
      missing_with_us <- !default_left
    }

    path <- c(
      path,
      list(list(
        type = "conditional",
        col = tree_df$split_feature[[parent_row]],
        val = tree_df$threshold[[parent_row]],
        op = op,
        missing = missing_with_us
      ))
    )

    # Move up the tree
    current_row <- parent_row
    current_parent_split <- tree_df$node_parent[[parent_row]]
  }

  rev(path) # Reverse to get root-to-leaf order
}
