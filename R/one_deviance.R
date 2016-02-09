#' Creates the deviance for one condition
#' \code{one_deviance} creates the deviance for one condition
#' one_deviance
#' @keywords internal
#' @export
one_deviance <- function(d, groups, loglikssaturated) {
  if (length(groups) == 0) loglikssaturated <- loglikssaturated$loglik
  else loglikssaturated <- semi_join(loglikssaturated, d, by = groups)$loglik
  deviance <- -2 * (d$loglik - loglikssaturated)
  data.frame(deviance)
}
