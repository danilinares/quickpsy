#' Creates the limits
#' \code{limits} creates the limits
#' @keywords internal
#' @importFrom rlang .data
limits <- function(averages, x_str, xmin, xmax) {

  one_limit <- function(averages, x_str, xmin, xmax) {
    if (is.null(xmin)) xmin <- min(averages[[x_str]])
    if (is.null(xmax)) xmax <- max(averages[[x_str]])
    data.frame(xmin, xmax)
  }

  averages %>% summarise(one_limit(.data, x_str, xmin, xmax), .groups = "keep")
}

