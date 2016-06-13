#' Calculates the AICs
#'
#' \code{aic} calculates the AICs.
#' @param qp output from quickpsy
#' @export
aic <- function(qp) {
  qp$logliks %>% do(one_aic(., qp$groups, qp$par))
}


