#' quickpsy
#' @export
quickpsy <- function(d, x = x, k = response, n, random, within, between,
                     xmin = NULL, xmax = NULL, log = F, fun = cum_normal_fun,
                     pini = NULL, guess = 0, lapses = 0, prob = NULL,
                     ypred = T, curves = T, thresholds = T, loglik = T,
                     boot = 'par', B = 1000, ci = .95, pini2 = NULL) {
print('quickpsy')

  x <- deparse(substitute(x))
  k <- deparse(substitute(k))
  fun <- deparse(substitute(fun))
  if (!missing(n)) n <- deparse(substitute(n))
  else n <- NULL
  if (!missing(random)) random <- as.character(substitute(random))[-1]
  if (!missing(within)) within <- as.character(substitute(within))[-1]
  if (!missing(between)) between <- as.character(substitute(between))[-1]

  ### calling the standard evaluation of quickpsy
  quickpsy_(d, x, k, n, random, within, between, xmin, xmax, log, fun, pini,
            guess, lapses, prob, ypred, curves, thresholds, loglik, boot,
            B, ci, pini2)
}


