#' Creates sequences of x's
#' @param limits The \code{limits} data frame from quickpsy.
#' @param x The \code{x} data frame from quickpsy.
#' @param grouping The \code{grouping} data frame from quickpsy.
#' @importFrom rlang .data
x_seq <- function(limits, x, grouping) {

  limits %>%
    summarise(!!quo_name(x) := seq(.data$xmin, .data$xmax, length = 500), .groups = "drop_last") %>%
    group_by(!!!syms(grouping))

}

