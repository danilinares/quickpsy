#' Calculates the thresholds
#' @keywords internal
#' \code{thresholds} Calculates the thresholds
#' @param qp output from quickpsy
#' @param prob Probability to calculate the threshold.
#' @param log Use \code{TRUE}, if the logarithm of the independent variable
#' has been used to fit the curves (default is \code{FALSE}).
#' @importFrom rlang .data
#' @importFrom stats approx
thresholds <- function(param, curves, funname, psych_fun, prob, log, guess, lapses, grouping){

  one_threshold <- function(param, curves, funname, prob, log, guess, lapses) {

    par <- param$par
    if (is.numeric(guess) && is.numeric(lapses))
      q <- (prob - guess) / (1 - guess - lapses)
    if (is.logical(guess) && is.logical(lapses))
      q <- (prob - par[3]) / (1 - par[3] - par[4])
    if (is.logical(guess) && is.numeric(lapses))
      q <- (prob - par[3]) / (1 - par[3] - lapses)
    if (is.numeric(guess) && is.logical(lapses))
      q <- (prob - guess) / (1 - guess - par[3])


    if (q < 0 || q > 1) {
      thre <- approx(curves$y,curves$x, xout = prob, ties = "ordered")$y
    }
    else {
      if (funname == "cum_normal_fun")
        thre <- inv_cum_normal_fun(q, c(par[1], par[2]))
      if (funname == "logistic_fun")
        thre <- inv_logistic_fun(q, c(par[1], par[2]))
      if (funname == "weibull_fun")
        thre <- inv_weibull_fun(q, c(par[1], par[2]))
    }

    data.frame(prob, thre)
  }

  one_threshold2 <- function(x, y, prob, log) {
    thre <- approx(y, x, xout = prob, ties = "ordered")$y

    data.frame(prob, thre)
  }


  if (is.null(prob)) {
    if (is.logical(guess) && guess) prob <- .5
    else  prob <- guess + .5 * (1 - guess)
  }


  if (funname != "no_default") {
    param_n <- param %>% nest_by(.key = "param")
    curves_n <- curves %>% nest_by(.key = "curves")

    param_n %>%
      left_join(curves_n, by = grouping) %>%
      group_by(!!!syms(grouping)) %>%
      rowwise() %>%
      summarise(one_threshold(param, curves, funname, prob, log, guess, lapses), .groups = "keep")
  }
  else {
    curves %>% summarise(one_threshold2(.data$x, .data$y, prob, log), .groups = "keep")
  }

}






