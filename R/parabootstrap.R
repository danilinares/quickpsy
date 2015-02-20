#' parabootstrap
#'
#' @export
parabootstrap <- function(qp, bootstrap = 'parametric', B = 1000) {
  qp$averages %>% dplyr::do(one_bootstrap(., qp$x, qp$k, qp$n,
                                          qp$psyfunguesslapses,
                                    qp$funname, qp$guess, qp$lapses,
                                    qp$pini, qp$piniset, qp$DE, bootstrap, B,
                                   qp$groups, qp$ypred))
}
