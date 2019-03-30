#' @export
tidypredict_fit.earth <- function(model) {
  parsedmodel <- parse_model(model)
  build_fit_formula(parsedmodel)
}

#' @export
parse_model.earth <- function(model) {

  if (any(names(model) == "terms")) {
    vars <- attr(model$terms, "dataClasses")
    vars_names <- names(vars)
  } else {
    vars_names <- model$namesx.org
  }

  is_glm <- !is.null(model$glm.list)

  all_coefs <- model$coefficients
  if(is_glm) all_coefs <- model$glm.coefficients

  coef_labels <- rownames(all_coefs)
  all_dirs <- rownames(model$dirs)
  all_terms <- map(
    model$selected.terms,
    ~ {
      list(
        label = all_dirs[.x],
        coef  = all_coefs[which(all_dirs[.x] == coef_labels)],
        is_intercept = ifelse(all_dirs[.x] == "(Intercept)", 1, 0),
        fields = get_fields(.x, model)
      )
    }
  )

  pm <- list()
  pm$general$model <- "earth"
  pm$general$version <- 2
  pm$general$is_glm <- 0
  if(is_glm){
    pm$general$is_glm <- 1
    fam <- model$glm.list[[1]]$family
    pm$general$family <- fam$family
    pm$general$link   <- fam$link
  }
  pm$terms <- all_terms

  pm
}

get_fields <- function(term_number, model){

  if (any(names(model) == "terms")) {
    vars_names <- names(attr(model$terms, "dataClasses"))
  } else {
    vars_names <- model$namesx.org
  }

  sel <- model$dirs[term_number, ]
  label <- colnames(model$dirs)[sel != 0]
  dirs <- model$dirs[term_number, sel != 0]
  cuts <- model$cuts[term_number, sel != 0]

  map(
    seq_along(label),
    ~{
      cl <- label[.x]
      subs <- map_chr(vars_names, ~substr(cl, 1, nchar(.x)))
      vals <- map_chr(vars_names, ~substr(cl, nchar(.x) + 1, nchar(cl)))
      subs <- subs[vars_names == subs]
      vals <- vals[vars_names == subs]
      if(vals != "") {
        list(
          type = "conditional",
          col = subs,
          val = vals,
          op = "equal"
        )
      } else {
        list(
          type = "operation",
          col = cl,
          val = cuts[.x][[1]],
          op = ifelse(dirs[.x][[1]] == -1, "lessthan", "morethan")
        )
      }

    }
  )
}
