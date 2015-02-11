#' bootstrap
#'
#' @export
bootstrap <- function(qp, bootstrap = 'par', B = 1000) {
  qp$averages %>% do(one_bootstrap(., qp$x, qp$k, qp$n, qp$psyfunguesslapses,
                                    qp$funname, qp$guess, qp$lapses,
                                    qp$pini, qp$pini2, B, qp$groups, qp$ypred))
}
