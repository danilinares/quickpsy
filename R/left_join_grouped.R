#' \code{left_join_grouped} left_join by a (possibly empty) set of grouping columns
#' @keywords internal
left_join_grouped <- function(x, y, by) {
  if (length(by) == 0) dplyr::cross_join(x, y)
  else dplyr::left_join(x, y, by = by)
}
