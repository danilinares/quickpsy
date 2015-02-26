#' Calculates the thresholds
#'
#' \code{thresholds} alculates the thresholds
#' @param qp output from quickpsy
#' @param prob Probability to calculate the threshold.
#' @param log Use \code{TRUE}, if the logarithm of the independent variable
#' has been used to fit the curves (default is \code{FALSE}).
#' @export
#' @examples
#' library(MPDiR) # contains the Vernier data
#' data(Vernier) # ?Venier for the reference
#' fit <- quickpsy(Vernier, Phaseshift, NumUpward, N,
#'                 grouping = .(Direction, WaveForm, TempFreq),
#'                 thresholds = F)
#' thresholds(fit, prob = .5)
#' @export
thresholds <- function(qp, prob = NULL, log = F) {
  if (is.null(prob)) stop('You need to specify the value of prob', call. = F)
  qp$para %>% do(one_threshold(., prob, log,
                               qp$funname, qp$guess, qp$lapses, qp$curves))
}



