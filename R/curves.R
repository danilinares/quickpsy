#' @keywords internal
#' @export
curves <- function(qp, xmin = NULL, xmax = NULL, log = F) {
  qp$par %>% do(one_curve(., xmin, xmax, log, qp$groups, qp$limits,
                           qp$psyfunguesslapses))

}
