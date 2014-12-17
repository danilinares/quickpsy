#' fit_psy
#'
#' @export

fit_psy <- function(d, x, k, n, psyfun, psyfunname,
                    pini, guess, lapses, DE, pini2) {
  #subd <- d %>% select_(paste0('-', x), paste0('-', k), paste0('-', n))
  #print(head(d,1))



  def_funs <- list(cum_normal_fun = cum_normal_fun,
                   logistic_fun = logistic_fun,
                   weibull_fun = weibull_fun)

  if (psyfunname %in%  names(def_funs)) {
    if (is.null(pini)) {
      pini <- calculate_pini(d, x, k, n, guess, lapses)
    }
  }

  cat('Initial parameters:', pini, '\n')
  cat('Guess:', guess,'   Lapses:',lapses, '\n----------------\n')
  psyfun <- create_psy_fun(psyfun, guess, lapses)
  para <- fit_main(d, x, k, n, psyfun, pini, DE, pini2)
  cat('Parameters:', para, '\n')

  handle_exceptions(psyfunname, para, guess, lapses)

  list(d = d, x = x, k = k, n = n,
       psyfunname = psyfunname , pini = pini, guess = guess, lapses = lapses,
       para = para,
       psyfun = psyfun)
}
