#' fit_main
#'
#' @export
fit_psy <- function(d, x, k, n, psy_fun_name, pini, guess, lapses) {
  if (is.character(psy_fun_name)) {
    pini <- calculate_pini(d, x, k, n, psy_fun_name, guess, lapses)
    psy_fun <- create_psy_fun(psy_fun_name, guess, lapses)
  }
  else if (is.function(psy_fun_name)){
    psy_fun <- psy_fun_name
  }
  cat('Initial parameters:', pini, '\n')
  para <- fit_main(d, x, k, n, psy_fun, pini)
  cat('Parameters:', para, '\n')
  handle_exceptions(psy_fun_name, para, guess, lapses)

  list(d = d, x = x, k = k, n = n,
       psy_fun_name =psy_fun_name , pini = pini, guess = guess, lapses = lapses,
       para = para,
       psy_fun = psy_fun)
}
