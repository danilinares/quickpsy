#' Fits psychometric functions.
#'
#' \code{quickpsy_} is the standard evaluation SE function associated
#' to the non-standard evaluation NSE function \code{quickpsy}.
#' \href{http://adv-r.had.co.nz/Computing-on-the-language.html}{SE functions can be more easily called from other functions.}
#' In SE functions, you need to quote the names of the variables.
#' @seealso \code{\link{quickpsy}}
#' @examples
#' library(MPDiR) # contains the Vernier data
#' data(Vernier) # ?Venier for the reference
#' fit <- quickpsy_(Vernier, 'Phaseshift', 'NumUpward', 'N',
#'                 grouping = c('Direction', 'WaveForm', 'TempFreq'))
#' plotcurves(fit)
#' @export
quickpsy_ <- function(d, x = 'x', k = 'k', n = 'n', grouping, random, within,
                      between, xmin = NULL, xmax = NULL, log = F,
                      fun = 'cum_normal_fun', pini = NULL, guess = 0,
                      lapses = 0, prob = NULL, thresholds = T,  logliks = F,
                      bootstrap = 'parametric', B = 100, ci = .95,
                      optimization = 'optim') {

  if (!is.null(prob)) thresholds <- T

  if (missing(n)) n <- NULL
  if (is.null(pini)) piniset <- F
  else piniset <- T

  qp <- fitpsy(d, x, k, n, random, within, between, grouping, xmin, xmax, log,
               fun, pini, piniset, guess, lapses, optimization)

  qp <- c(qp, list(piniset=piniset))

  qp <- c(qp, list(ypred = ypred(qp)))

  if (sum(qp$ypred$ypred < 0) + sum(qp$ypred$ypred > 1) > 0)
    if (bootstrap == 'parametric')
      stop ('As y-predictions are not within (0,1), bootstrap should be \'nonparametric\'', call.=F)

  qp <- c(qp, list(curves = curves(qp, log = log)))

  if (thresholds) {
    if (is.null(prob))
      if (is.logical(guess) && guess) prob <- .5
      else  prob <- guess + .5 * (1 - guess)
    qp <- c(qp, list(thresholds = as.data.frame(thresholds(qp, prob, log))))
  }

  if (logliks) qp <- c(qp, list(logliks = logliks(qp)))
  if (bootstrap == 'parametric' || bootstrap == 'nonparametric') {
    qp <- c(qp, list(parabootstrap = parabootstrap(qp, bootstrap, B)))
    qp <- c(qp, list(paraci = as.data.frame(paraci(qp, ci))))
    if (thresholds) {
      qp <- c(qp, list(thresholdsbootstrap = thresholdsbootstrap(qp, prob, log)))
      qp <- c(qp, list(thresholdsci = as.data.frame(thresholdsci(qp, ci))))
    }
  }
  else if (bootstrap != 'none')
    stop('Bootstrap should be \'parametric\', \'nonparametric\' or \'none\'.', call. = F)
  class(qp) <- 'quickpsy'

  if (log) qp$averages[[x]] <- exp(qp$averages[[x]])

  qp
}


