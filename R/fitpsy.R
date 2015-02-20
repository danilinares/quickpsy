#' fitpsy
#' @export
fitpsy <- function(d, x, k, n, random, within, between, xmin, xmax, log,
                    funname, pini, piniset, guess, lapses, DE) {

  fun <- get(funname)

  if (is.logical(guess) && !guess) guess <- 0
  if (is.logical(lapses) && !lapses) lapses <- 0

  d <- d  %>% dplyr::ungroup()
  groups <- c()
  if (!missing(random)) groups <- c(groups, random)
  if (!missing(within)) groups <- c(groups, within)
  if (!missing(between)) groups <- c(groups, between)
  if (is.null(n)) {
    d[[k]][d[[k]] == -1] <- 0
    d <- d %>% dplyr::group_by_(.dots=c(groups, x)) %>%
      dplyr::summarise_(n = 'n()', k = paste0('sum(',k,')'))
    names(d)[names(d) == 'k'] <- k
    n <- 'n'
  }

  if (!(missing(random) && missing(within) && missing(between)))
    d <- d %>% dplyr::group_by_(.dots=groups)

  d$y <- d[[k]] / d[[n]]

  if (log) d[[x]] <- log(d[[x]])

  psyfunguesslapses <- create_psy_fun(fun, guess, lapses)

  limits <- limits(d, x, xmin, xmax, log)

  groups <- as.character(groups(d))

  if (!piniset)
    if (funname %in% names(get_functions()))
      pini <- pini(d, x, k, n, guess, lapses, funname)
    else stop('Initial parameters should be set.')

  para <- parameters(d, x, k, n, psyfunguesslapses, funname,
                     pini, piniset, guess, lapses, DE, groups)

  list(x = x, k = k , n = n, guess = guess, lapses = lapses, averages = d,
       groups = groups, funname = funname,
       psyfunguesslapses = psyfunguesslapses, limits = limits, pini = pini,
       DE = DE, para = para)
}


