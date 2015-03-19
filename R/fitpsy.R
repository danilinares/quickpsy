#' Fits the curve
#' \code{fitpsy} fits de curve
#' @keywords internal
#' @export
fitpsy <- function(d, x, k, n, random, within, between, grouping, xmin, xmax,
                   log, funname, parini, pariniset, guess, lapses, optimization) {

  fun <- get(funname)

  if (is.logical(guess) && !guess) guess <- 0
  if (is.logical(lapses) && !lapses) lapses <- 0

  d <- d  %>% ungroup()
  groups <- c()
  if (!missing(random)) groups <- c(groups, random)
  if (!missing(within)) groups <- c(groups, within)
  if (!missing(between)) groups <- c(groups, between)
  if (!missing(grouping)) groups <- c(groups, grouping)
  if (is.null(n)) {
    d[[k]][d[[k]] == -1] <- 0
    d <- d %>% group_by_(.dots=c(groups, x)) %>%
      summarise_(n = 'n()', k = paste0('sum(',k,')'))
    names(d)[names(d) == 'k'] <- k
    n <- 'n'
  }

  if (!(missing(random) && missing(within) && missing(between) && missing(grouping)))
    d <- d %>% group_by_(.dots=groups)

  d$y <- d[[k]] / d[[n]]

  if (log) d[[x]] <- log(d[[x]])

  psyfunguesslapses <- create_psy_fun(fun, guess, lapses)

  limits <- limits(d, x, xmin, xmax, log)

  groups <- as.character(groups(d))

  if (!pariniset)
    if (funname %in% names(get_functions()))
      parini <- parini(d, x, k, n, guess, lapses, funname)
    else stop('Initial parameters should be set.')

  par <- parameters(d, x, k, n, psyfunguesslapses, funname,
                     parini, pariniset, guess, lapses, optimization, groups)

  list(x = x, k = k , n = n, guess = guess, lapses = lapses, averages = d,
       groups = groups, funname = funname, log = log,
       psyfunguesslapses = psyfunguesslapses, limits = limits, parini = parini,
       optimization = optimization, par = par)
}


