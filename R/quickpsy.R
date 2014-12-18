#' quickpsy
#' @export
quickpsy <- function(d = data, x = x, k = response, n = NULL, random, within, between,
                     xmin = NULL, xmax = NULL, log = F,
                     psyfun = cum_normal_fun, pini = NULL,
                     guess = 0, lapses = 0, DE = F, pini2 = NULL,
                     threprob = NULL,
                     curv = T, thre = T, plotcurves = T) {

  handle_excep_quickpsy(DE, pini, pini2)

  if (plotcurves) {
    curv <- T
    thre <- T
  }

  if (is.logical(guess) && !guess) guess <- 0
  if (is.logical(lapses) && !lapses) lapses <- 0

  if (is.null(threprob))
    if (is.logical(guess) && guess) threprob <- .5
    else  threprob <- guess + .5 * (1 - guess)

  d <- d  %>% ungroup()
  x <- deparse(substitute(x))
  k <- deparse(substitute(k))
  if (!is.null(n)) n <- deparse(substitute(n))

  psyfunname <- deparse(substitute(psyfun))

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

  if (is.null(n)) {
    d[[k]][d[[k]] == -1] <- 0
    d <- d %>% group_by_(.dots=c(grouping_var, x)) %>%
      summarise_(n = 'n()', k = paste0('sum(',k,')'))
    names(d)[names(d) == 'k'] <- k
    n <- 'n'
    if (!(missing(random) && missing(within) && missing(between)) ) {
      d <- d %>% group_by_(.dots=grouping_var)
    }
  }
  else {
    if (!(missing(random) && missing(within) && missing(between)) )
      d <- d %>% group_by_(.dots=grouping_var)
  }

  d$y <- d[[k]] / d[[n]]

  fits <- par_psy(d, x, k, n, xmin , xmax, log, psyfun, psyfunname, pini,
          guess, lapses, DE, pini2)

  fitsGroups <- list(d = d, x = x, threprob = threprob, guess = guess,
                     fits = fits, grouping_var = grouping_var)
  out <- fitsGroups


  if (curv) out <- c(out, list(curves = curves(fitsGroups, xmin, xmax, log)))
  if (thre) out <- c(out, list(thresholds = thresholds(fitsGroups, threprob)))

  class(out) <- 'quickpsy'
  if (plotcurves) {
    grid.arrange(plotcurves(out),plotthre(out))
  }
  out

}


