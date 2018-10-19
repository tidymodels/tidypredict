strip_factor <- function(x) gsub("factor\\((.+)\\)", "\\1", x)

get_marker <- function() "{:}"

get_marker_regx <- function() "\\{\\:\\}"
