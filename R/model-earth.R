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
    vars_names <- rownames(model$modvars)
  }

  is_glm <- !is.null(model$glm.list)
  
  pm <- list()
  pm$general$model <- "earth"
  pm$general$type <- "regression"
  pm$general$version <- 2
  pm$general$is_glm <- 0
  if (is_glm) {
    pm$general$is_glm <- 1
    fam <- model$glm.list[[1]]$family
    pm$general$family <- fam$family
    pm$general$link <- fam$link
  }
  pm$terms <- mars_terms(model, is_glm)
  as_parsed_model(pm)
}


mars_terms <- function(mod, is_glm) { 
  feature_types <- 
    tibble::as_tibble(mod$dirs, rownames = "feature") %>% 
    dplyr::mutate(feature_num = dplyr::row_number()) %>% 
    tidyr::pivot_longer(cols = c(-feature, -feature_num),
                        values_to = "type",
                        names_to = "term")
  
  feature_values <- 
    tibble::as_tibble(mod$cuts, rownames = "feature") %>% 
    dplyr::mutate(feature_num = dplyr::row_number()) %>% 
    tidyr::pivot_longer(cols = c(-feature, -feature_num),
                        values_to = "value",
                        names_to = "term")
  
  if (is_glm)  {
    all_coefs <- mod$glm.coefficients
  } else {
    all_coefs <- mod$coefficients  
  }

  feature_coefs <- 
    # Note coef(mod) formats data differently for logistic regression
    tibble::as_tibble(all_coefs, rownames = "feature") %>% 
    setNames(c("feature", "coefficient"))
  
  term_to_column <- 
    tibble::as_tibble(mod$modvars, rownames = "column") %>% 
    tidyr::pivot_longer(cols = c(-column),
                        values_to = "value",
                        names_to = "term") %>% 
    purrr::transpose() %>% 
    purrr::map(~ {
      if(.x$value == 1) {
        .x$level <- gsub(.x$column, "", .x$term)  
      } else {
        .x$level <- NA
      }
      .x
    }) %>% 
    dplyr::bind_rows() %>% 
    dplyr::filter(value == 1) %>% 
    dplyr::select(-value)
  
  feature_types %>% 
    dplyr::full_join(feature_values, by = c("feature", "feature_num", "term")) %>% 
    dplyr::filter(type != 0) %>% 
    dplyr::right_join(feature_coefs,  by = "feature") %>% 
    dplyr::mutate(feature_num = ifelse(feature == "(Intercept)", 0, feature_num)) %>%
    dplyr::arrange(feature_num) %>% 
    dplyr::left_join(term_to_column, by = "term") %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(lists = list(make_lists(type, column, value, level))) %>% 
    dplyr::group_by(feature, feature_num) %>% 
    dplyr::summarize(final = collapse_lists(feature, coefficient, lists), .groups = "drop") %>% 
    purrr::pluck("final")
  
}


make_lists <- function(type, column, value, level) {
  if (is.na(type)) {
    return(list())
  }
  if (abs(type) == 1) {
    out <- operation_list(type, column, value)
  } else {
    out <- conditional_list(column, level)
  }
  out
}

conditional_list <- function(column, level) {
  if (nchar(level) == 0) {
    out <- list(
      type = "ordinary",
      col = column
    )
  } else {
    out <- list(
      type = "conditional",
      col = column,
      val = level,
      op = "equal"
    )
  }
  out
}

operation_list <- function(direction, column, split) {
  list(
    type = "operation",
    col = column,
    val = split,
    op = ifelse(direction < 0, "lessthan", "morethan")
  )
}

collapse_lists <- function(label, coef, lst) {
  label <- unique(label)
  coef <- unique(coef)
  if (length(label)  >1 ){
    browser()
  }
  list(
    list(
      label = label,
      coef = coef,
      is_intercept = ifelse(label == "(Intercept)", 1, 0),
      fields = lst
    )
  )
}