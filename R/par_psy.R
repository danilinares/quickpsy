#' par_psy
#' @export
par_psy <- function(d, x, k, n,
                     xmin, xmax, log,
                     psyfun = cum_normal_fun, psyfunname, pini,
                     guess, lapses, DE, pini2) {

  if (log) d[[x]] <- log(d[[x]])

  fits <- d %>%
    do(fit=fit_psy(., x, k, n, psyfun, psyfunname,
                   pini, guess = guess, lapses = lapses, DE, pini2))
  fits
}


