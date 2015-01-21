#' quickpsy_
#' @export
quickpsy_ <- function(d, x = 'x', k = 'response', n, random, within, between,
                      xmin = NULL, xmax = NULL, log = F, psyfun = 'cum_normal_fun',
                      pini = NULL, guess = 0, lapses = 0, prob = NULL,
                      curves = T, thresholds = T, DE = F, pini2 = NULL) {

  if (missing(n)) n <- NULL

  ### calling the standard evaluation of fitpsy
  qp <- fitpsy_(d, x, k, n, random, within, between, xmin, xmax, log,
          psyfun, pini, guess, lapses, DE, pini2)

  qp <- c(qp, list(x = x, groups = as.character(groups(qp$averages))))

  if (curves) qp <- c(qp, list(curves = curves(qp)))

  if (thresholds) {
    if (is.null(prob))
      if (is.logical(guess) && guess) prob <- .5
      else  prob <- guess + .5 * (1 - guess)
    qp <- c(qp, list(thresholds = thresholds(qp, prob)))
  }
  qp
}


