#' quickpsy
#' @export
quickpsy <- function(d, x, k, n, random, within, between, psy_fun_name,
                     pini = NULL ,guess = 0, lapses = 0) {
  x <- deparse(substitute(x))
  k <- deparse(substitute(k))
  n <- deparse(substitute(n))
  random <- deparse(substitute(random))
  within <- deparse(substitute(within))
  between <- deparse(substitute(between))

  d <- d %>% group_by_(random, within, between)

  fits <- d %>%
    do(fit=fit_psy(., x, k, n, psy_fun_name, pini, guess=guess, lapses=lapses))

  curve <-  plyr::ddply(fits,c(random, within, between),curve_psy)
  para <-  plyr::ddply(fits,c(random, within, between),para_psy)

  list(curve = curve , para = para)
}
