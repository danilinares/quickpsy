#' Creates bootstrap samples of the parameters
#'
#' \code{parabootstrap} creates bootstrap samples of the parameters.
#' @param qp output from quickpsy
#' @param bootstrap \code{'parametric'} performs parametric bootsrap;
#' \code{'nonparametric'} performs non-parametric bootstrap;
#' \code{'none'} does not perform bootstrap (default is \code{'parametric'}).
#' @param B number of bootstrap samples (default is ONLY a 100).
#' @export
#' @examples
#' library(MPDiR) # contains the Vernier data
#' data(Vernier) # ?Venier for the reference
#' fit <- quickpsy(Vernier, Phaseshift, NumUpward, N,
#'                 grouping = .(Direction, WaveForm, TempFreq),
#'                 bootstrap = 'none')
#' boot_samples <- parabootstrap(fit)
#' head(boot_samples)
#' @export
parabootstrap <- function(qp, bootstrap = 'parametric', B = 100) {
  qp$averages %>% dplyr::do(one_bootstrap(., qp$x, qp$k, qp$n,
                  qp$psyfunguesslapses, qp$funname, qp$guess, qp$lapses,
                  qp$pini, qp$piniset, qp$optimization, bootstrap, B,
                  qp$groups, qp$ypred))
}
