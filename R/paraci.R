#' paraci
#'
#' @export
paraci <- function(qp, ci = .95, method = 'percent') {
  qp$parabootstrap %>% group_by_(.dots = c(qp$groups, 'paran')) %>%
    do(one_paraci(., ci, method))
}
