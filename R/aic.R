#' Calculates the AICs
#'
#' \code{aic} calculates the AICs.
#' @param qp output from quickpsy
#' @export
#' @examples
#' library(MPDiR) # contains the Vernier data
#' fit <- quickpsy(Vernier, Phaseshift, NumUpward, N,
#'                 grouping = .(Direction, WaveForm, TempFreq), B = 20)
#' aic(fit)
#' @export
aic <- function(qp) {
  qp$logliks %>% do(one_aic(., qp$groups, qp$par))
}


