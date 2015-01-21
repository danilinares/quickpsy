#' fitpsy_
#' @export
fitpsy_ <- function(d, x, k, n, random, within, between, xmin, xmax, log,
                    psyfun, pini,guess, lapses, DE, pini2) {

  psyfunction <- get(psyfun)

  if (DE && (is.null(pini) || is.null(pini2))) stop('DEoptim requires pini (vector with the lower bounds of the initial values of the parameters) and pini2 (vector with the upper bounds ofthe initial values of the parameters).', call. = F)

  if (is.logical(guess) && !guess) guess <- 0
  if (is.logical(lapses) && !lapses) lapses <- 0

  d <- d  %>% ungroup()
  groups <- c()
  if (!missing(random)) groups <- c(groups, random)
  if (!missing(within)) groups <- c(groups, within)
  if (!missing(between)) groups <- c(groups, between)
  if (is.null(n)) {
    d[[k]][d[[k]] == -1] <- 0
    d <- d %>% group_by_(.dots=c(groups, x)) %>%
      summarise_(n = 'n()', k = paste0('sum(',k,')'))
    names(d)[names(d) == 'k'] <- k
    n <- 'n'
  }
  if (!(missing(random) && missing(within) && missing(between)))
    d <- d %>% group_by_(.dots=groups)

  d$y <- d[[k]] / d[[n]]

  if (log) d[[x]] <- log(d[[x]])

  psyfunguesslapses <- create_psy_fun(psyfunction, guess, lapses)

  limits <- d %>% do(calculate_limits(., x, xmin, xmax, log))

  param <- d %>% do(one_fit(., x, k, n, psyfunguesslapses, psyfun,
                     pini, guess = guess, lapses = lapses, DE, pini2))

  list(guess = guess, lapses = lapses, averages = d, fun = psyfunguesslapses,
       psyfun = psyfun, limits = limits, param = param)
}


