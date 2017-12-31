#' Converts an R model object into a table
#'
#' It parses a fitted R model's structure to extract all of the necessary
#' components to pass along to the functions that create the actual
#' Tidy Eval formula that dplyr can read.  It is currently able to parse
#' lm() and glm() models.
#'
#' @param model An R model object
#'
#' @examples
#'
#' df <- data.frame(x = c(1, 2, 5, 6 ,6), y = c(2, 3, 6, 5, 4))
#' model <- lm(x ~ y, df)
#' parse_model(model)
#'
#' @export
parse_model <- function(model) {
  UseMethod("parse_model")
}
#' @export
parse_model.lm <- function(model) parse_model_lm(model)

#' @export
parse_model.glm <- function(model) parse_model_lm(model)

#' @import dplyr
#' @importFrom tibble tibble
add_variable <- function(df, labels, vals) {
  df %>%
    bind_rows(tibble(
      labels = !! labels,
      vals = as.character(!! vals),
      type = "variable"
    ))
}

#' @import rlang
#' @importFrom purrr map2
#' @importFrom purrr map
#' @importFrom purrr reduce
#' @import dplyr
#' @importFrom tibble tibble
#' @importFrom tibble rowid_to_column
#' @importFrom tibble rownames_to_column
#' @importFrom utils head
#' @importFrom stats predict
#' @importFrom stats qt
parse_model_lm <- function(model) {
  acceptable_formula(model)

  terms <- model$terms

  labels <- attr(terms, "term.labels")

  tidy <- tibble(labels)

  xl <- model$xlevels

  if (length(xl) > 0) {
    xlevels <- names(xl) %>%
      map({
        ~tibble(
          labels = .x,
          vals = as.character(xl[[.x]])
        ) %>%
          rowid_to_column("row")
      }) %>%
      bind_rows() %>%
      filter(row > 1) %>%
      select(-row)

    tidy <- tidy %>%
      left_join(xlevels, by = "labels")
  }

  i <- attr(terms, "intercept")
  if (!is.null(i)) {
    tidy <-
      tibble(
        labels = "(Intercept)",
        vals = ""
      ) %>%
      bind_rows(tidy)
  }

  tidy <- tidy %>%
    mutate(
      vals = ifelse(is.na(vals), "", vals),
      type = case_when(
        labels == "(Intercept)" ~ "intercept",
        vals == "" ~ "continuous",
        vals != "" ~ "categorical",
        TRUE ~ "error"
      )
    )

  tidy <- add_variable(tidy, labels = "model", vals = class(model)[[1]])
  tidy <- add_variable(tidy, labels = "residual", vals = model$df.residual)

  if (length(summary(model)$sigma ^ 2) > 0) {
    tidy <- add_variable(tidy, labels = "sigma2", vals = summary(model)$sigma ^ 2)
  }

  if (!is.null(model$family$family)) {
    tidy <- add_variable(tidy, labels = "family", vals = model$family$family)
  }

  if (!is.null(model$family$link)) {
    tidy <- add_variable(tidy, labels = "link", vals = model$family$link)
  }

  offset <- model$call$offset
  if (!is.null(offset)) {
    tidy <- tidy %>%
      bind_rows(tibble(
        labels = "offset",
        vals = as.character(offset),
        type = "variable"
      ))
  }

  coef <- summary(model)$coefficients %>%
    as.data.frame() %>%
    rownames_to_column("coef_labels") %>%
    select(
      coef_labels,
      estimate = Estimate
    )

  res.var <- summary(model)$sigma ^ 2

  qr <- qr.solve(qr.R(model$qr)) %>%
    as.data.frame() %>%
    rownames_to_column()

  colnames(qr) <- c("coef_labels", paste0("qr_", 1:nrow(qr)))

  tidy <- tidy %>%
    mutate(coef_labels = paste0(labels, vals)) %>%
    full_join(coef, by = "coef_labels") %>%
    full_join(qr, by = "coef_labels") %>%
    select(-coef_labels)

  if (any(is.na(tidy$labels))) {
    stop("Error parsing the model")
  }

  tidy
}

#' @import rlang
#' @importFrom purrr map2
#' @importFrom purrr map
#' @importFrom purrr reduce
#' @import dplyr
#' @export
parse_model.randomForest <- function(model) {
  model_frame <- getTree(model, labelVar = TRUE) %>%
    as.tibble() %>%
    rowid_to_column()

  colnames(model_frame) <- sub(" ", "_", colnames(model_frame))

  all_paths <- model_frame %>%
    filter(status == -1) %>%
    pull(rowid) %>%
    map(~get_path(.x, model_frame)) %>%
    bind_rows()

  tidy <- model_frame %>%
    filter(status == -1) %>%
    rowid_to_column("labels") %>%
    mutate(
      labels = paste0("path-", labels),
      type = "path",
      estimate = 0
    ) %>%
    select(labels, vals = prediction, type, estimate) %>%
    bind_cols(all_paths) %>%
    add_row(labels = "model", vals = "randomForest", type = "variable")

  tidy
}

get_marker <- function() "{:}"


get_path <- function(row_id, model_frame) {
  field <- NULL
  operator <- NULL
  split_point <- NULL

  for (get_path in 1:nrow(model_frame)) {
    current <- filter(model_frame, rowid == row_id)
    if (current$status == 1) {
      field <- c(
        field,
        as.character(current$split_var)
      )

      operator <- c(
        operator,
        paste0(ifelse(to_left, "left", "right"))
      )

      split_point <- c(
        split_point,
        as.character(current$split_point)
      )
    }

    left <- which(model_frame$left_daughter == row_id)
    right <- which(model_frame$right_daughter == row_id)
    parent <- as.numeric(paste0(left, right, collapse = ""))
    to_left <- length(left) > 0

    if (is.na(parent)) {
      path <- tibble(
        field = paste0(field, collapse = get_marker()),
        operator = paste0(operator, collapse = get_marker()),
        split_point = paste0(split_point, collapse = get_marker())
      )
      break
    }

    row_id <- parent
  }
  path
}
