#' fit_psy
#'
#' @export

fit_psy <- function(d, x, k, n, psy_fun, psy_fun_name,
                    pini, guess, lapses, DE, pini2) {
  #subd <- d %>% select_(paste0('-', x), paste0('-', k), paste0('-', n))
  #print(head(d,1))

  def_funs <- list(cum_normal_fun = cum_normal_fun,
                   logistic_fun = logistic_fun)

  if (psy_fun_name %in%  names(def_funs)) {
    if (is.null(pini)) {
      pini <- calculate_pini(d, x, k, n, psy_fun, guess, lapses)
    }
  }

  cat('Initial parameters:', pini, '\n')
  cat('Guess:', guess,'\nLapses:',lapses, '\n')
  psy_fun <- create_psy_fun(psy_fun, guess, lapses)
  para <- fit_main(d, x, k, n, psy_fun, psy_fun_name, pini, DE, pini2)
  cat('Parameters:', para, '\n')

  handle_exceptions(psy_fun_name, para, guess, lapses)

  list(d = d, x = x, k = k, n = n,
       psy_fun_name =psy_fun_name , pini = pini, guess = guess, lapses = lapses,
       para = para,
       psy_fun = psy_fun)
}
