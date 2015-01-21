#' fitpsy
#' @export
fitpsy <- function(d = data, x = x, k = response, n = NULL, random, within, between,
                     xmin = NULL, xmax = NULL, log = F,
                     psyfun = cum_normal_fun, pini = NULL,
                     guess = 0, lapses = 0, DE = F, pini2 = NULL) {
  ### deparsing
  x <- deparse(substitute(x))
  k <- deparse(substitute(k))
  if (!is.null(n)) n <- deparse(substitute(n))

  if (!missing(random)) random <- as.character(substitute(random))[-1]
  if (!missing(within)) within <- as.character(substitute(within))[-1]
  if (!missing(between)) between <- as.character(substitute(between))[-1]

  fitpsy_(d, x, k, n, random, within, between, xmin, xmax, log,
          psyfun, pini, guess, lapses, DE, pini2)
}


