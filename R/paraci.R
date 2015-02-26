#' @keywords internal
#' @export
paraci <- function(qp, ci = .95) {
  qp$parabootstrap %>% dplyr::group_by_(.dots = c(qp$groups, 'paran')) %>%
    dplyr::do(one_paraci(., ci))
}
