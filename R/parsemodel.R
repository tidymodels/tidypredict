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

parse_model_lm <- function(model) {
  acceptable_formula(model)

  var_labels <- names(attr(model$terms, "dataClasses"))
  if (attr(model$terms, "response") == 1) var_labels <- var_labels[2:length(var_labels)]

  vars <- tibble(var = var_labels)

  xl <- model$xlevels
  if (length(xl) > 0) {
    xl_df <- 1:length(xl) %>%
      map_df(~tibble(
        var = names(xl[.x]),
        vals = xl[[.x]]
      ))
    vars <- vars %>%
      left_join(xl_df, by = "var") %>%
      mutate(fullname = paste0(.data$var, ifelse(is.na(.data$vals), "", .data$vals)))
  } else {
    vars <- vars %>%
      mutate(fullname = .data$var)
  }

  co <- model$coefficients

  est <- names(co) %>%
    map(~strsplit(.x, ":"))

  est_df <- seq_len(length(est)) %>%
    map_df(~tibble(
      coefno = .x,
      fullname = est[[.x]][[1]]
    ))

  all_vals <- est_df %>%
    left_join(vars, by = "fullname") %>%
    mutate(vals = ifelse(.data$fullname == .data$var, "{{:}}", .data$vals)) %>%
    filter(!is.na(.data$var)) %>%
    filter(!is.na(.data$vals)) %>%
    select(-.data$fullname) %>%
    group_by(.data$coefno) %>%
    spread(.data$var, .data$vals)

  new_vals <- as_list(colnames(all_vals))
  names(new_vals) <- colnames(all_vals)

  all_vals <- as_tibble(new_vals) %>%
    mutate(coefno = 0L) %>%
    bind_rows(all_vals)

  colnames(all_vals) <- c("coefno", paste0("field_", (2:length(all_vals)) - 1))

  tidy <- as_tibble(model$coefficients) %>%
    rownames_to_column("labels") %>%
    rowid_to_column("coefno") %>%
    rename(estimate = .data$value) %>%
    mutate(type = "term") %>%
    bind_rows(tibble(
      coefno = 0,
      labels = "labels",
      estimate = 0,
      type = "variable"
    )) %>%
    left_join(all_vals, by = "coefno")

  qr <- qr.solve(qr.R(model$qr)) %>%
    as.data.frame() %>%
    rownames_to_column()

  colnames(qr) <- c("coef_labels", paste0("qr_", 1:nrow(qr)))

  cf <- as_list(c("labels", rep(NA, length(qr) - 1)))
  names(cf) <- names(qr)

  qr <- qr %>%
    bind_rows(as_tibble(cf))

  tidy <- tidy %>%
    bind_cols(qr) %>%
    mutate(label_match = .data$coef_labels != .data$labels)

  if (sum(tidy$label_match) == 0) {
    tidy <- tidy %>%
      select(
        -.data$coefno,
        -.data$coef_labels,
        -.data$label_match
      )
  } else {
    stop("There was a parsing error")
  }

  tidy <- add_variable(tidy, labels = "model", vals = class(model)[[1]])
  tidy <- add_variable(tidy, labels = "version", vals = "1.0")
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

  tidy
}

#' @export
parse_model.randomForest <- function(model) {
  model_frame <- randomForest::getTree(model, labelVar = TRUE) %>%
    as.tibble() %>%
    rowid_to_column()

  colnames(model_frame) <- sub(" ", "_", colnames(model_frame))

  all_paths <- model_frame %>%
    filter(.data$status == -1) %>%
    pull(.data$rowid) %>%
    map(~get_path(.x, model_frame)) %>%
    bind_rows()

  tidy <- model_frame %>%
    filter(.data$status == -1) %>%
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
    add_row(labels = "model", vals = "randomForest", type = "variable")

  tidy
}

get_marker <- function() "{:}"

get_path <- function(row_id, model_frame) {
  field <- NULL
  operator <- NULL
  split_point <- NULL

  for (get_path in 1:nrow(model_frame)) {
    current <- filter(model_frame, .data$rowid == row_id)
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