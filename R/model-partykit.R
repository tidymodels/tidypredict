partykit_tree_info <- function(model) {
  model_nodes <- map(seq_along(model), ~ model[[.x]])
  is_split <- map_lgl(model_nodes, ~ class(.x$node[1]) == "partynode")
  # non-cat model
  mean_resp <- map_dbl(model_nodes, ~ mean(.x$fitted[, "(response)"]))
  prediction <- ifelse(!is_split, mean_resp, NA)
  party_nodes <- map(seq_along(model), ~ partykit::nodeapply(model, .x))
  
  kids <- map(party_nodes, ~ {
    if (length(.x[[1]]$kids)) {
      map(.x[[1]]$kids, ~ .x$id)
    }
  })
  vars <- as.character(attr(model$terms, "variables"))
  vars <- vars[2:length(vars)]
  
  var_details <- map_chr(model$data, class)
  var_class <- as.character(var_details)
  var_name <- names(var_details)
  
  splitvarID <- map_int(model_nodes, ~ ifelse(is.null(.x$node$split$varid), NA, .x$node$split$varid))
  
  if(length(var_class) > 0) {
    class_splits <- map_chr(seq_along(splitvarID), ~{
      if(is.na(splitvarID[.x])) return(NA)
      v <- vars[splitvarID[.x]]
      if(var_class[var_name == v] == "factor") {
        lvls <- levels(model$data[, colnames(model$data) == v])
        pn <- party_nodes[[.x]][[1]]$split$index 
        pn <- ifelse(is.na(pn), 0, pn)
        if(any(pn == 3)) stop("Three levels are not supported")
        paste0(lvls[pn == 1], collapse =", ")
      } else {
        NA
      }
    })
  } else {
    class_splits <- NA
  }
  
  data.frame(
    nodeID = seq_along(is_split) - 1,
    leftChild = map_int(kids, ~ ifelse(is.null(.x[[1]]), NA, .x[[1]])) - 1,
    rightChild = map_int(kids, ~ ifelse(is.null(.x[[2]]), NA, .x[[2]])) - 1,
    splitvarID,
    splitvarName = vars[splitvarID],
    splitval = map_dbl(model_nodes, ~ ifelse(is.null(.x$node$split$breaks), NA, .x$node$split$breaks)),
    splitclass = class_splits,
    terminal = !is_split,
    prediction
  )
}

get_pk_tree <- function(model) {
  tree <- partykit_tree_info(model)
  paths <- tree$nodeID[tree[, "terminal"]]
  map(
    paths,
    ~ {
      prediction <- tree$prediction[tree$nodeID == .x]
      if (is.null(prediction)) stop("Prediction column not found")
      if (is.factor(prediction)) prediction <- as.character(prediction)
      list(
        prediction = prediction,
        path = get_ra_path(.x, tree, FALSE)
      )
    }
  )
}

#' @export
parse_model.party <- function(model) {
  classes <- attr(model$terms, "dataClasses")
  pm <- list()
  pm$general$model <- "party"
  pm$general$type <- "tree"
  pm$general$version <- 2
  pm$trees <- list(get_pk_tree(model))
  as_parsed_model(pm)
}

# Fit formula -----------------------------------

#' @export
tidypredict_fit.party <- function(model) {
  parsedmodel <- parse_model(model)
  build_fit_formula_rf(parsedmodel)[[1]]
}
