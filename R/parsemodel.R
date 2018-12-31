#' Converts an R model object into a table
#'
#' It parses a fitted R model's structure and extracts the components
#' needed to create a dplyr formula for prediction.  The function also
#' creates a data frame using an specific format so that other
#' functions in the future can also pass parsed tables to a given
#' formula creating function.
#'
#' @param model An R model object. It currently supports lm(),
#' glm() and randomForest() models.
#'
#' @examples
#' library(dplyr)
#' df <- mutate(mtcars, cyl = paste0("cyl", cyl))
#' model <- lm(mpg ~ wt + cyl * disp, offset = am, data = df)
#' parse_model(model)
#'
#' @export
parse_model <- function(model) {
  UseMethod("parse_model")
}

# ranger() models -----------------------------------------

#' @export
parse_model.ranger <- function(model) {
  model_frame <- ranger::treeInfo(model) %>%
    as.tibble() %>%
    mutate(rowid = .data$nodeID) %>%
    rename_all(tolower)

  all_paths <- model_frame %>%
    filter(is.na(.data$leftchild), is.na(.data$rightchild)) %>%
    pull(.data$rowid) %>%
    map(~get_path_ranger(.x, model_frame)) %>%
    bind_rows()

  tidy <- model_frame %>%
    as.tibble() %>%
    filter(is.na(.data$leftchild), is.na(.data$rightchild)) %>%
    rowid_to_column("labels") %>%
    mutate(
      labels = paste0("path-", labels),
      type = "path",
      estimate = 0
    ) %>%
    mutate(vals = .data$prediction) %>%
    select(
      .data$labels,
      .data$vals,
      .data$type,
      .data$estimate
    ) %>%
    bind_cols(all_paths) %>%
    add_row(labels = "model", vals = "ranger", type = "variable")

  tidy
}


get_path_ranger <- function(row_id, model_frame) {
  field <- NULL
  operator <- NULL
  split_point <- NULL
  current_val <- row_id

  for (get_path in row_id:1) {
    current <- model_frame[get_path, ]
    if (!is.na(current$leftchild)) {
      if (current$leftchild == current_val || current$rightchild == current_val) {
        field <- c(
          field,
          as.character(current$splitvarname)
        )
        operator <- c(
          operator,
          if (current$leftchild == current_val) "left" else "right"
        )
        split_point <- c(
          split_point,
          as.character(current$splitval)
        )
        current_val <- current$rowid
      }
    }
  }
  tibble(
    field = paste0(field, collapse = get_marker()),
    operator = paste0(operator, collapse = get_marker()),
    split_point = paste0(split_point, collapse = get_marker())
  )
}



# Helper functions ----------------------------------------

#' @import dplyr
#' @importFrom tibble tibble
add_variable <- function(df, labels, vals) {
  df %>%
    bind_rows(tibble(
      labels = !!labels,
      vals = as.character(!!vals),
      type = "variable"
    ))
}

get_marker <- function() "{:}"

get_marker_regx <- function() "\\{\\:\\}"
