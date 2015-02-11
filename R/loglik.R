#' loglik
#'
#' @export
loglik <- function(qp) {
print('loglik')
  qp$averages %>%
    do(one_loglik(., qp$x, qp$k, qp$n, qp$psyfunguesslapses, qp$groups,
                  qp$para))
}


