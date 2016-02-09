#' Calculates the deviances
#'
#' \code{deviance} calculates the deviances.
#' @param qp output from quickpsy
#' @export
#' @examples
#' library(MPDiR) # contains the Vernier data
#' fit <- quickpsy(Vernier, Phaseshift, NumUpward, N,
#'                 grouping = .(Direction, WaveForm, TempFreq), B = 20)
#' deviance(fit)
#' @export
deviance <- function(qp) {
  qp$logliks %>% do(one_deviance(., qp$groups, qp$loglikssaturated))
}


