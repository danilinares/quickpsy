#' fitpsy_
#' @export
fitpsy_backup <- function(d = data, x = x, k = response, n = NULL, random, within, between,
                     xmin = NULL, xmax = NULL, log = F,
                     psyfun = cum_normal_fun, pini = NULL,
                     guess = 0, lapses = 0, DE = F, pini2 = NULL) {

  handle_excep_quickpsy(DE, pini, pini2)

  ### setting arguments
  if (is.logical(guess) && !guess) guess <- 0
  if (is.logical(lapses) && !lapses) lapses <- 0
#   if (is.null(threprob))
#     if (is.logical(guess) && guess) threprob <- .5
#     else  threprob <- guess + .5 * (1 - guess)

  out <- list(guess = guess, lapses = lapses)

  ### deparsing
  x <- deparse(substitute(x))
  k <- deparse(substitute(k))
  if (!is.null(n)) n <- deparse(substitute(n))
  psyfunname <- deparse(substitute(psyfun))

  ### grouping
  d <- d  %>% ungroup()
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
  }
  if (!(missing(random) && missing(within) && missing(between)) )
    d <- d %>% group_by_(.dots=grouping_var)

  d$y <- d[[k]] / d[[n]]

  out <- c(out, list(averages = d))

  if (log) d[[x]] <- log(d[[x]])

  ### psychometric function with guess and lapses
  psyfunguesslapses <- create_psy_fun(psyfun, guess, lapses)
  out <- c(out, list(fun = psyfunguesslapses, psyfunname = psyfunname))

  ###  limits of x
  limits <- d %>% do(calculate_limits(., x, xmin, xmax, log))
  out <- c(out, list(limits = limits))

  ### parameters
  param <- d %>% do(do_fit(., x, k, n, psyfunguesslapses, psyfunname,
                     pini, guess = guess, lapses = lapses, DE, pini2))
  out <- c(out, list(param = param))

  out


#   fitsGroups <- list(d = d, x = x, threprob = threprob, guess = guess,
#                      fits = fits, grouping_var = grouping_var)
#   out <- fitsGroups
#
#   if (curv) out <- c(out, list(curves = curves(fitsGroups, xmin, xmax, log)))
#   if (thre) out <- c(out, list(thresholds = thresholds(fitsGroups, threprob)))
#
#   class(out) <- 'quickpsy'
#   if (plotcurves) {
#     grid.arrange(plotcurves(out),plotthre(out))
#   }
#   out

}


