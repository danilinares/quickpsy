#' Creates sequences of x's
#' @param limits The \code{limits} data frame from quickpsy.
#' @param x The \code{x} data frame from quickpsy.
#' @param grouping The \code{grouping} data frame from quickpsy.
#' @param line_res Specify the number of points to draw the curves.
#' @importFrom rlang .data
x_seq <- function(limits, x, grouping, line_res) {

  limits %>%
    summarise(!!quo_name(x) := seq(.data$xmin, .data$xmax, length = line_res), .groups = "drop_last") %>%
    group_by(!!!syms(grouping))

}

