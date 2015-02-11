#' quickpsy_
#' @export
quickpsy_ <- function(d, x = 'x', k = 'response', n, random, within, between,
                      xmin = NULL, xmax = NULL, log = F, fun = 'cum_normal_fun',
                      pini = NULL, guess = 0, lapses = 0, prob = NULL,
                      ypred = T, curves = T, thresholds = T,  loglik = T,
                      bootstrap = 'par', B = 1000, ci = .95, pini2 = NULL) {
print('quickpsy_')

  if (missing(n)) n <- NULL

  qp <- fitpsy(d, x, k, n, random, within, between, xmin, xmax, log,
               fun, pini, guess, lapses, pini2)

  if (ypred) qp <- c(qp, list(ypred = ypred(qp)))

  if (curves) qp <- c(qp, list(curves = curves(qp)))

  if (thresholds) {
    if (is.null(prob))
      if (is.logical(guess) && guess) prob <- .5
      else  prob <- guess + .5 * (1 - guess)
    qp <- c(qp, list(thresholds = thresholds(qp, prob)))
  }

  qp <- c(qp, list(loglik = loglik(qp)))

  if (bootstrap == 'par' || bootstrap == 'nonpar') {
    if (bootstrap == 'par')
      qp <- c(qp, list(parabootstrap = bootstrap(qp, bootstrap, B)))

    qp <- c(qp, list(thresholdsbootstrap = thresholdsbootstrap(qp, prob)))
    qp <- c(qp, list(paraci = paraci(qp, ci)))
   qp <- c(qp, list(thresholdsci = thresholdsci(qp, ci)))
  }


  qp
}


