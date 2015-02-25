#' @keywords internal
#' @export
limits <- function(d, x, xmin, xmax, log) {
  d %>% dplyr::do(one_limit(., x, xmin, xmax, log))
}
