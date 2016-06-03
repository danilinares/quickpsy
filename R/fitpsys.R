#' Fits the curves
#' \code{fitpsys} fits de curves
#' @keywords internal
#' @export
fitpsys <- function(d, x, k, n, random, within, between, grouping, xmin, xmax,
          log, funname, parini, optimization) {



  d <- d  %>% ungroup()
  groups <- c()
  if (!missing(random)) groups <- c(groups, random)
  if (!missing(within)) groups <- c(groups, within)
  if (!missing(between)) groups <- c(groups, between)
  if (!missing(grouping)) groups <- c(groups, grouping)

  print(groups)


  if (is.null(n)) {
    d[[k]][d[[k]] == -1] <- as.numeric(0)
    d <- d %>% group_by_(.dots=c(groups, x)) %>%
      summarise_(n = 'n()', k = paste0('sum(',k,')'))
    names(d)[names(d) == 'k'] <- k
    n <- 'n'
  }

  if (!(missing(random) && missing(within) && missing(between) && missing(grouping)))
    d <- d %>% group_by_(.dots=groups)

  prob <- NULL
  d$prob <- d[[k]] / d[[n]]

  if (log) d[[x]] <- log(d[[x]])



  limits <- limits(d, x, xmin, xmax, log)

  groups <- as.character(groups(d))

  if (is.null(parini)) stop('parini (initial parameters) must be specified.')


  par <- parameters(d, x, k, n, psyfunguesslapses, funname,
                     parini, pariniset, guess, lapses, optimization, groups)

  list(x = x, k = k , n = n, guess = guess, lapses = lapses,
       averages = d,
       groups = groups, funname = funname, log = log,
       psyfunguesslapses = psyfunguesslapses, limits = limits,
       pariniset = pariniset, parini = parini,
       optimization = optimization, par = par)
}


