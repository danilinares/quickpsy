#' Creates the curves
#' \code{curves} creates the curves
#' @import dplyr
#' @keywords internal
#' @export
. <- 'No te quejes'
curves <- function(qp, xmin = NULL, xmax = NULL, log = F) {
  qp$par tidyr::%>% do(one_curve(., xmin, xmax, log, qp$groups, qp$limits,
                           qp$psyfunguesslapses))

}
