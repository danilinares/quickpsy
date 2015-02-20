#' curves
#'
#' @export
curves <- function(qp, xmin = NULL, xmax = NULL, log = F) {
print('curves')
  qp$para %>% dplyr::do(one_curve(., xmin, xmax, log, qp$groups, qp$limits,
                           qp$psyfunguesslapses))

}
