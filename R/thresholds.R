#' thresholds
#'
#' @export
thresholds <- function(qp, prob = NULL, log = F) {

  one_thre <- function(d, psyfun, prob, guess, lapses) {
    para <- d$param
    log <- F
    def_funs <- list(cum_normal_fun = cum_normal_fun, ## Hay que meter el def_funs solo en un sitio
                     logistic_fun = logistic_fun, weibull_fun = weibull_fun)

    if (psyfun %in%  names(def_funs)) {
      if (is.numeric(guess) && is.numeric(lapses))
        q <- (prob - guess) / (1 - guess - lapses)
      if (is.logical(guess) && is.logical(lapses))
        q <- (prob - para[3]) / (1 - para[3] - para[4])
      if (is.logical(guess) && is.numeric(lapses))
        q <- (prob - para[3]) / (1 - para[3] - lapses)
      if (is.numeric(guess) && is.logical(lapses))
        q <- (prob - guess) / (1 - guess - para[3])

      if (psyfun == 'cum_normal_fun')
        thre <- inv_cum_normal_fun(q, c(para[1], para[2]))
      if (psyfun == 'logistic_fun')
        thre <- inv_logistic_fun(q, c(para[1], para[2]))
      if (psyfun == 'weibull_fun')
        thre <- inv_weibull_fun(q, c(para[1], para[2]))
    }
    #if (log) thre <- exp(thre)
    data.frame(thre, prob)
  }

  qp$param %>% do(one_thre(.,qp$psyfun, prob, qp$guess, qp$lapses))
}



