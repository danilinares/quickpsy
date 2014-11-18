#' fit_main
#'
#' @export
fit_psy <- function(d, x, k, n, psy_fun_name, pini = NULL, guess, lapses) {
  x <- deparse(substitute(x))
  k <- deparse(substitute(k))
  n <- deparse(substitute(n))
  if (is.character(psy_fun_name)) {
    pini <- calculate_pini(d, x, k, n, psy_fun_name, guess, lapses)

    psy_fun <- create_psy_fun(psy_fun_name, guess, lapses)
  }
  else if (is.function(psy_fun_name)){
    psy_fun <- psy_fun_name
  }
  cat('Initial parameters:',pini)
  para <- fit_main(d, x, k, n, psy_fun, pini)
  manage_exceptions(psy_fun_name, para, guess, lapses)
  list(para = para,
       psy_fun = psy_fun)
}
