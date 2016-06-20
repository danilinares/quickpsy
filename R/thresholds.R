#' Calculates the thresholds
#' @keywords internal
#' \code{thresholds} Calculates the thresholds
#' @param qp output from quickpsy
#' @param prob Probability to calculate the threshold.
#' @param log Use \code{TRUE}, if the logarithm of the independent variable
#' has been used to fit the curves (default is \code{FALSE}).
#' @export

thresholds <- function(qp, prob = NULL, log = FALSE) {
  if (is.null(prob)) stop('You need to specify the value of prob', call. = FALSE)
    qp$par %>% do(one_threshold(., prob, log, qp$groups,
                               qp$funname, qp$guess, qp$lapses, qp$curves))
}



