#' Calculates the loglikelihoods
#' \code{logliks} calculates the loglikelihoods.
#' @param qp output from quickpsy
#' @export
logliks <- function(qp) {
  qp$averages %>%
    do(one_loglik(., qp$x, qp$k, qp$n, qp$psyfunguesslapses, qp$groups,
                  qp$par))
}


