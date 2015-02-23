#' ypred
#'
#' @export
ypred <- function(qp) {
  qp$para %>% dplyr::do(one_ypred(., log, qp$groups, qp$averages, qp$x,
                           qp$psyfunguesslapses))

}
