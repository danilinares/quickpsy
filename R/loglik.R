#' loglik
#'
#' @export
loglik <- function(qp) {
print('loglik')
  qp$averages %>%
    dpyr::do(one_loglik(., qp$x, qp$k, qp$n, qp$psyfunguesslapses, qp$groups,
                  qp$para))
}


