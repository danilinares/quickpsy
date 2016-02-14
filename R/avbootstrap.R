#' Creates bootstrap samples 
#'
#' \code{avbootstrap} creates bootstrap samples
#' @param qp output from quickpsy
#' @param bootstrap \code{'parametric'} performs parametric bootstrap;
#' \code{'nonparametric'} performs non-parametric bootstrap;
#' \code{'none'} does not perform bootstrap (default is \code{'parametric'}).
#' @param B number of bootstrap samples (default is 100 ONLY).
#' @examples
#' library(MPDiR) # contains the Vernier data
#' data(Vernier) # ?Venier for the reference
#' fit <- quickpsy(Vernier, Phaseshift, NumUpward, N,
#'                 grouping = .(Direction, WaveForm, TempFreq),
#'                 bootstrap = 'none')
#' boot_samples <- parbootstrap(fit)
#' head(boot_samples)
#' @export
avbootstrap <- function(qp, bootstrap = 'parametric', B = 100) {
  if (qp$pariniset) {
    if (is.atomic(parini)) {
      parini <- qp$par
      pariniset <- FALSE
    }
    else{
      parini <- qp$parini
      pariniset <- TRUE
    }
  }
  else {
    parini <- qp$par
    pariniset <- FALSE
  }

  qp$averages %>% do(one_bootstrapav(., qp$x, qp$k, qp$n,
                  qp$psyfunguesslapses, qp$funname, qp$guess, qp$lapses,
                  parini, pariniset, qp$optimization, bootstrap, B,
                  qp$groups, qp$ypred))
}
