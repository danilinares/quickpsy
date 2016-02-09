#' Calculates the saturated loglikelihoods
#'
#' \code{loglikssaturated} calculates the saturated loglikelihoods.
#' @param qp output from quickpsy
#' @export
#' @examples
#' library(MPDiR) # contains the Vernier data
#' fit <- quickpsy(Vernier, Phaseshift, NumUpward, N,
#'                 grouping = .(Direction, WaveForm, TempFreq), B = 20)
#' loglikssaturated(fit)
#' @export
loglikssaturated <- function(qp) {
  qp$averages %>%
    do(one_logliksaturated(., qp$x, qp$k, qp$n, qp$psyfunguesslapses, qp$groups,
                  qp$par))
}


