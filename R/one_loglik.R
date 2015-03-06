#' one_loglik
#' @keywords internal
#' @export
one_loglik <- function(d, x, k, n, psyfunguesslapses, groups, par) {
  nllfun <- create_nll(d, x, k, n, psyfunguesslapses)
  if (length(groups) == 0) par <- par$par
  else par <- dplyr::semi_join(par, d, by = groups)$par
  data.frame(loglik = -nllfun(par))
}
