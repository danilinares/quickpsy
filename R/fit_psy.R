#' @export
fit_psy <- function(d, x, k, n, psy_fun, pini, guess, lapses) {
  x <- deparse(substitute(x))
  k <- deparse(substitute(k))
  n <- deparse(substitute(n))
  if (is.character(psy_fun)) psy_fun <- create_psy_fun(psy_fun,guess,lapses)
  para <- fit_main(d, x, k, n, psy_fun, pini)
  list(para = para,
       psy_fun = psy_fun)
}
