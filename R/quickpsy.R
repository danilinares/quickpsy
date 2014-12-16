#' quickpsy
#' @export
quickpsy <- function(d, x, k, n, random, within, between,
                     xmin = NULL, xmax = NULL, log = F,
                     psyfun = cum_normal_fun, pini = NULL,
                     guess = 0, lapses = 0, DE = F, pini2 = NULL,
                     threprob = .5 * (1 - guess),
                     curv = T, thre = T) {

  handle_excep_quickpsy(DE, pini, pini2)

  d <- d  %>% ungroup()
  x <- deparse(substitute(x))
  k <- deparse(substitute(k))
  n <- deparse(substitute(n))
  psyfunname <- deparse(substitute(psyfun))

  d$y <- d[[k]] / d[[n]]


  grouping_var <- c()
  if (!missing(random)) {
    random <- as.character(substitute(random))[-1]
    grouping_var <- c(grouping_var, random)
  }
  if (!missing(within)) {
    within <- as.character(substitute(within))[-1]
    grouping_var <- c(grouping_var, within)
  }
  if (!missing(between)) {
    between <- as.character(substitute(between))[-1]
    grouping_var <- c(grouping_var, between)
  }
  if (!(missing(random) && missing(within) && missing(between)) ) {
    d <- d %>% group_by_(.dots=grouping_var)
  }

  fits <- par_psy(d, x, k, n, xmin , xmax, log, psyfun, psyfunname, pini,
          guess, lapses, DE, pini2)

  fitsGroups <- list(d = d, x = x, threprob = threprob, guess = guess,
                     fits = fits, grouping_var = grouping_var, threprob = threprob)
  out <- fitsGroups

  if (curv) out <- c(out, list(curves = curves(fitsGroups)))
  if (thre) out <- c(out, list(thresholds = thresholds(fitsGroups, threprob)))

  class(out) <- 'quickpsy'
  out

}


