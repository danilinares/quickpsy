#' quickpsy
#' @export
quickpsy <- function(d, x, k, n, random, within, between,
                     xmin = NULL, xmax = NULL, log = F,
                     psy_fun = cum_normal_fun, pini = NULL,
                     guess = 0, lapses = 0, DE = F, pini2 = NULL) {

  if (DE && (is.null(pini) || is.null(pini2))) stop('DEoptim requires pini (vector with the lower bounds of the initial values of the parameters) and pini2 (vector with the upper bounds of the initial values of the parameters) ')
  d <- ungroup(d)
  x <- deparse(substitute(x))
  k <- deparse(substitute(k))
  n <- deparse(substitute(n))
  psy_fun_name <- deparse(substitute(psy_fun))
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

  if (log) d[[x]] <- log(d[[x]])

  fits <- d %>%
    do(fit=fit_psy(., x, k, n, psy_fun, psy_fun_name,
                   pini, guess = guess, lapses = lapses, DE, pini2))

  curve <-  plyr::ddply(fits,grouping_var, function(d) curve_psy(d, xmin, xmax, log))
  para <-  plyr::ddply(fits,grouping_var, function(d) para_psy(d))

  list(curve = curve , para = para)
}


