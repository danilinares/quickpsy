#' quickpsy_
#' @export
quickpsy_ <- function(d, x = 'x', k = 'response', n, random, within, between,
                      xmin = NULL, xmax = NULL, log = F, fun = 'cum_normal_fun',
                      pini = NULL, guess = 0, lapses = 0, prob = NULL,
                      ypred = T, thresholds = T,  loglik = F,
                      bootstrap = 'parametric', B = 30, ci = .95,
                      DE = F) {

  if (missing(n)) n <- NULL
  if (is.null(pini)) piniset <- F
  else piniset <- T

  qp <- fitpsy(d, x, k, n, random, within, between, xmin, xmax, log,
               fun, pini, piniset, guess, lapses, DE)

  qp <- c(qp, list(piniset=piniset))

  if (ypred) qp <- c(qp, list(ypred = ypred(qp)))

  if (sum(qp$ypred$ypred < 0) + sum(qp$ypred$ypred > 1) > 0)
    if (bootstrap == 'parametric')
      stop ('y-predictions not within (0,1) , bootstrap should be nonparametric')


  qp <- c(qp, list(curves = curves(qp)))

  if (thresholds) {
    if (is.null(prob))
      if (is.logical(guess) && guess) prob <- .5
      else  prob <- guess + .5 * (1 - guess)
    qp <- c(qp, list(thresholds = thresholds(qp, prob)))
  }

  if (loglik) qp <- c(qp, list(loglik = loglik(qp)))

  if (bootstrap == 'parametric' || bootstrap == 'nonparametric') {
    qp <- c(qp, list(parabootstrap = parabootstrap(qp, bootstrap, B)))
    qp <- c(qp, list(thresholdsbootstrap = thresholdsbootstrap(qp, prob)))
    qp <- c(qp, list(paraci = paraci(qp, ci)))
    qp <- c(qp, list(thresholdsci = thresholdsci(qp, ci)))
  }

  qp
}


