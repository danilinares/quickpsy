#' ypred
#'
#' @export
ypred <- function(qp) {
print('ypred')
  qp$para %>% do(one_ypred(., log, qp$groups, qp$averages, qp$x,
                           qp$psyfunguesslapses))

}
