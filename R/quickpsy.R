#' quickpsy
#' @export
quickpsy <- function(d, x, k, n, random, within, between, psy_fun,
                     pini = NULL, guess = 0, lapses = 0, DE = F, pini2 = NULL) {

  if (DE && (is.null(pini) || is.null(pini2))) stop('DEoptim requires pini (vector with the lower bounds of the initial values of the parameters) and pini2 (vector with the upper bounds of the initial values of the parameters) ')
  d <- ungroup(d)
  x <- deparse(substitute(x))
  k <- deparse(substitute(k))
  n <- deparse(substitute(n))
  psy_fun_name <- deparse(substitute(psy_fun))
  grouping_var <- c()

  if (!missing(random)) {
    random <- deparse(substitute(random))
    grouping_var <- c(grouping_var, random)
  }
  if (!missing(within)) {
    within <- deparse(substitute(within))
    grouping_var <- c(grouping_var, within)
  }
  if (!missing(between)) {
    between <- deparse(substitute(between))
    grouping_var <- c(grouping_var, between)
  }
  if (!(missing(random) && missing(within) && missing(between)) ) {
    d <- d %>% group_by_(.dots=grouping_var)
  }

  fits <- d %>%
    do(fit=fit_psy(., x, k, n, psy_fun, psy_fun_name,
                   pini, guess = guess, lapses = lapses, DE, pini2))

  curve <-  plyr::ddply(fits,grouping_var, curve_psy)
  para <-  plyr::ddply(fits,grouping_var, para_psy)

  list(curve = curve , para = para)
}


