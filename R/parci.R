#' @keywords internal
#' @export
parci <- function(qp, ci = .95) {
  qp$parbootstrap %>% group_by_(.dots = c(qp$groups, 'parn')) %>%
    do(one_parci(., ci))
}
