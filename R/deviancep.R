#' Calculates the probability of obtaining the deviance value or larger if the parametric model holds
#' \code{deviancep} calculates the probability of obtaining the deviance value or larger if the parametric model holds.
#' @keywords internal
#' @export
deviancep <- function(qp) {
  qp$devianceboot %>% group_by_(.dots = c(qp$groups)) %>%
    do(one_deviancep(., qp$groups, qp$deviance))
}
