#' quickpsy
#' @export
quickpsy <- function(d, x = x, k = response, n, random, within, between,
                     xmin = NULL, xmax = NULL, log = F, psyfun = cum_normal_fun,
                     pini = NULL, guess = 0, lapses = 0, prob = NULL,
                     curves = T, thresholds = T, DE = F, pini2 = NULL) {

  x <- deparse(substitute(x))
  k <- deparse(substitute(k))
  psyfun <- deparse(substitute(psyfun))
  if (!missing(n)) n <- deparse(substitute(n))
  else n <- NULL
  if (!missing(random)) random <- as.character(substitute(random))[-1]
  if (!missing(within)) within <- as.character(substitute(within))[-1]
  if (!missing(between)) between <- as.character(substitute(between))[-1]

  ### calling the standard evaluation of quickpsy
  quickpsy_(d, x, k, n, random, within, between, xmin, xmax, log, psyfun, pini,
            guess, lapses, prob, curves, thresholds, DE, pini2)
}


