#' @import rlang
#' @importFrom purrr map2
#' @importFrom purrr map
#' @importFrom purrr imap
#' @importFrom purrr map_chr
#' @importFrom purrr map_lgl
#' @importFrom purrr map_dbl
#' @importFrom purrr map_int
#' @importFrom purrr map_dfr
#' @importFrom purrr reduce
#' @importFrom purrr transpose
#' @importFrom utils head
#' @importFrom stats predict
#' @importFrom stats qt
#' @importFrom tibble tibble
#' @importFrom knitr knit_print
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr %>%
#' @importFrom stats binomial
#' @importFrom stats setNames
#' @keywords internal
#' @importFrom generics tidy
#' 
"_PACKAGE"
NULL
utils::globalVariables(
  c(
    ".", "pm", "coefficient", "column", "feature", "feature_num", "level",
    "lists", "type", "value"
  )
)
