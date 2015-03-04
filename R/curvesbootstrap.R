#' @keywords internal
#' @export
curvesbootstrap <- function(qp, xmin = NULL, xmax = NULL, log = F) {
  if (length(qp$groups) == 0)
    paraboot <- qp$parabootstrap %>% dplyr::group_by_('sample')
  else
    paraboot <- qp$parabootstrap %>%
                dplyr::group_by_(.dots = c(qp$groups, 'sample'))

  paraboot %>% dplyr::do(one_curve(., xmin, xmax, log, qp$groups, qp$limits,
                                   qp$psyfunguesslapses))

}



