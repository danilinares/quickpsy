#' Calculates the AIC for one condition
#' \code{one_aic} creates the deviance for one condition
#' one_deviance
#' @keywords internal
#' @export
one_aic <- function(d, groups, par) {
  k <- length(unique(par$parn))
  aic <- -2 * d$loglik + 2 * k
  data.frame(aic)
}
