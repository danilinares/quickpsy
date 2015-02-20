#' one_threshold
#'
#' @export
one_threshold <- function(d, prob, log, funname, guess, lapses, curves) {
  para <- d$para

  if (funname %in%  names(get_functions())) {
    if (is.numeric(guess) && is.numeric(lapses))
      q <- (prob - guess) / (1 - guess - lapses)
    if (is.logical(guess) && is.logical(lapses))
      q <- (prob - para[3]) / (1 - para[3] - para[4])
    if (is.logical(guess) && is.numeric(lapses))
      q <- (prob - para[3]) / (1 - para[3] - lapses)
    if (is.numeric(guess) && is.logical(lapses))
      q <- (prob - guess) / (1 - guess - para[3])

    if (q < 0 || q > 1) {
      warning('probabilities not whitin 0 and 1')
      thre <- approx(curves$y,curves$x, xout= prob)$y
    }
    else {
      if (funname == 'cum_normal_fun')
        thre <- inv_cum_normal_fun(q, c(para[1], para[2]))
      if (funname == 'logistic_fun')
        thre <- inv_logistic_fun(q, c(para[1], para[2]))
      if (funname == 'weibull_fun')
        thre <- inv_weibull_fun(q, c(para[1], para[2]))
    }
  }
  #if (log) thre <- exp(thre)


  data.frame(thre, prob)
}





