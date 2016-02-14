#' Calculates the probability of obtaining the deviance value or larger if the
#' parametric model holds for one condition
#' \code{one_deviancep} calculates the probability of obtaining the deviance value
#' or larger if the parametric model holds for one condition
#' @keywords internal
#' @export
one_deviancep <- function(d, groups, deviance) {
  if (length(groups) != 0) deviance <- semi_join(deviance, d, by = groups)
  empiricalF <- ecdf(d$deviance)
  p <- 1 - empiricalF(deviance$deviance)
  data.frame(p)
}

