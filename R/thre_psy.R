#' thre_psy
#'
#' @export
thre_psy <- function(d, threprob) {
  f <- d$fit[[1]]
  para <- f$para
  psyfunname <- f$psyfunname
  guess <- f$guess
  lapses <- f$lapses
  log<-F

  def_funs <- list(cum_normal_fun = cum_normal_fun, ## Hay que meter el def_funs solo en un sitio
                   logistic_fun = logistic_fun, weibull_fun = weibull_fun)

  if (psyfunname %in%  names(def_funs)) {

      if (is.numeric(guess) && is.numeric(lapses))
        q <- (threprob - guess) / (1 - guess - lapses)
      if (is.logical(guess) && is.logical(lapses))
        q <- (threprob - para[3]) / (1 - para[3] - para[4])
      if (is.logical(guess) && is.numeric(lapses))
        q <- (threprob - para[3]) / (1 - para[3] - lapses)
      if (is.numeric(guess) && is.logical(lapses))
        q <- (threprob - guess) / (1 - guess - para[3])


      if (psyfunname == 'cum_normal_fun')
        thre <- inv_cum_normal_fun(q, c(para[1], para[2]))
      if (psyfunname == 'logistic_fun')
        thre <- inv_logistic_fun(q, c(para[1], para[2]))
      if (psyfunname == 'weibull_fun')
        thre <- inv_weibull_fun(q, c(para[1], para[2]))


  }

  if (log) thre <- exp(thre)

  data.frame(thre)
}
