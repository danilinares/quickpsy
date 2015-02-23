#' loglik
#'
#' @export
logliks <- function(qp) {
  qp$averages %>%
    dpyr::do(one_loglik(., qp$x, qp$k, qp$n, qp$psyfunguesslapses, qp$groups,
                  qp$para))
}


