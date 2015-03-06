#' @keywords internal
#' @export
parci <- function(qp, ci = .95) {
  qp$parbootstrap %>% dplyr::group_by_(.dots = c(qp$groups, 'parn')) %>%
    dplyr::do(one_parci(., ci))
}
