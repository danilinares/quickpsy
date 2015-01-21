#' fit_main
#'
#' @inheritParams create_nll
#' @param pini Initial parameters.
#' @export
fit_main <- function(d, x, k, n, psyfun, pini, DE, pini2) {
  nll <- create_nll(d, x, k, n, psyfun)
  if (DE) {
    mod <- DEoptim(nll,lower=pini,upper=pini2)$optim
    para <- mod$bestmem
  }
  else para <- optim(pini, nll)$p
}

