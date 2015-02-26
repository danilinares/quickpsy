#' Creates bootstrap samples of the thresholds.
#'
#' \code{thresholdsbootstrap} creates bootstrap samples of the thresholds.
#' @param qp output from quickpsy
#' @param prob Probability to calculate the threshold.
#' @param log Use \code{TRUE}, if the logarithm of the independent variable
#' has been used to fit the curves (default is \code{FALSE}).
#' @examples
#' library(MPDiR) # contains the Vernier data
#' data(Vernier) # ?Venier for the reference
#' fit <- quickpsy(Vernier, Phaseshift, NumUpward, N,
#'                 grouping = .(Direction, WaveForm, TempFreq),
#'                 thresholds = F)
#' boot_samples_thre <- thresholdsbootstrap(fit, prob = .5)
#' head(boot_samples_thre)
#' @export
thresholdsbootstrap <- function(qp, prob = NULL, log = F) {
  if (is.null(prob)) stop('You need to specify the value of prob', call. = F)
  if (length(qp$groups) == 0)
    qp$parabootstrap %>% group_by_('sample') %>%
      do(one_threshold(., prob, log, qp$funname, qp$guess, qp$lapses))
  else
    qp$parabootstrap %>% group_by_(.dots = c(qp$groups, 'sample')) %>%
      do(one_threshold(., prob, log, qp$funname, qp$guess, qp$lapses))
}



