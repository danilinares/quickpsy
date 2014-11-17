#' fit_main
#'
#' @inheritParams create_nll
#' @param pini Initial parameters.
#' @export
fit_main <- function(d, x, k, n, psy_fun, pini) {
  nll <- create_nll(d, x, k, n, psy_fun)
  para <- optim(pini, nll)$p
}
