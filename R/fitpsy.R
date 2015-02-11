#' fitpsy
#' @export
fitpsy <- function(d, x, k, n, random, within, between, xmin, xmax, log,
                    funname, pini, guess, lapses, pini2) {

  fun <- get(funname)

  if (!is.null(pini2) && is.null(pini)) stop('pini2 corresponds to the upper bound of the range for minimization; pini2 (the lower bound) is then required.', call. = F)

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

  psyfunguesslapses <- create_psy_fun(fun, guess, lapses)

  limits <- limits(d, x, xmin, xmax, log)

  groups <- as.character(groups(d))

  if (funname %in% names(get_functions()))
    if (is.null(pini)) pini <- pini(d, x, k, n, guess, lapses, funname)


  para <- parameters(d, x, k, n, psyfunguesslapses, funname,
                     pini, guess, lapses, pini2, groups)

  list(x = x, k = k , n = n, guess = guess, lapses = lapses, averages = d,
       groups = groups, funname = funname,
       psyfunguesslapses = psyfunguesslapses, limits = limits, pini = pini,
       pini2 = pini2, para = para)
}


