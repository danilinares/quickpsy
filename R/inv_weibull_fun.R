#' Inverse weibull  function
#'
#' Inverse weibull function
#' @param prob Vector of probabilities.
#' @param p Vector of parameters \code{p = c(\alpha, \beta)}.
#' @return \code{x} at each probability.
#' @examples
#' yseq <- seq(0, 1, .01)
#' xseq <- inv_weibull_fun(yseq, c(2, 4))
#' curve <- data.frame(x = xseq, y = yseq)
#' ggplot(curve, aes(x = x, y = y)) + geom_line()
#' @export
inv_weibull_fun <- function(x, p) qweibull(x, p[2], p[1])
