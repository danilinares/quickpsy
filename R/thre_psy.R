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
                   logistic_fun = logistic_fun)

  if (psyfunname %in%  names(def_funs)) {

      if (is.numeric(guess) && is.numeric(lapses))
        if (psyfunname == 'cum_normal_fun')
          thre <- inv_cum_normal_fun((threprob - guess) / (1 - guess - lapses), c(para[1], para[2]))
       # if (psyfunname == 'logistic_fun')

      if (is.logical(guess) && is.logical(lapses))
        if (psyfunname == 'cum_normal_fun')
          thre <- inv_cum_normal_fun((threprob - p[3]) / (1 - p[3] - p[4]), c(para[1], para[2]))

      if (is.logical(guess) && is.numeric(lapses))
        if (psyfunname == 'cum_normal_fun')
          thre <- inv_cum_normal_fun((threprob - p[3]) / (1 - p[3] - lapses), c(para[1], para[2]))

      if (is.numeric(guess) && is.logical(lapses))
        if (psyfunname == 'cum_normal_fun')
          thre <- inv_cum_normal_fun((threprob - guess) / (1 - guess - p[3]), c(para[1], para[2]))

  if (log) thre <- exp(thre)
  }

  data.frame(thre)
}
