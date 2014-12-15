#' thre_psy
#'
#' @export
thre_psy <- function(l, threprob, psyfunname, guess, lapses, log) {
  f <- l$fit[[1]]
  para <- f$para

  def_funs <- list(cum_normal_fun = cum_normal_fun, ## Hay que meter el def_funs solo en un sitio
                   logistic_fun = logistic_fun)

  if (psyfunname %in%  names(def_funs)) {
    if (psyfunname == 'cum_normal_fun') {
      if (is.numeric(guess) && is.numeric(lapses))
        thre <- qnorm((threprob - guess) / (1 - guess - lapses), para[1], para[2])

      if (is.logical(guess) && is.logical(lapses))
        thre <- qnorm((threprob - p[3]) / (1 - p[3] - p[4]), para[1], para[2])

      if (is.logical(guess) && is.numeric(lapses))
        thre <- qnorm((threprob - p[3]) / (1 - p[3] - lapses), para[1], para[2])

      if (is.numeric(guess) && is.logical(lapses))
        thre <- qnorm((threprob - guess) / (1 - guess - p[3]), para[1], para[2])
    }
  if (log) thre <- exp(thre)
  }

  data.frame(thre)
}
