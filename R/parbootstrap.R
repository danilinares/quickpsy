#' Creates bootstrap samples of the parameters
#'
#' \code{parbootstrap} creates bootstrap samples of the parameters.
#' @param qp output from quickpsy
#' @examples
#' library(MPDiR) # contains the Vernier data
#' data(Vernier) # ?Venier for the reference
#' fit <- quickpsy(Vernier, Phaseshift, NumUpward, N,
#'                 grouping = .(Direction, WaveForm, TempFreq),
#'                 bootstrap = 'none')
#' boot_samples <- parbootstrap(fit)
#' head(boot_samples)
#' @export
parbootstrap <- function(qp) {
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

  if (length(qp$groups) == 0)
    avboot <- qp$avbootstrap %>% group_by_('sample')
  else
    avboot <- qp$avbootstrap %>%
      group_by_(.dots = c(qp$groups, 'sample'))

  avboot %>%
    do(one_parameters(., qp$x, qp$k, qp$n, qp$psyfunguesslapses, qp$funname,
                      parini, pariniset, qp$guess, qp$lapses,
                      qp$optimization, qp$groups))

}


