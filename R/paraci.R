#' paraci
#'
#' @export
paraci <- function(qp, ci = .95, method = 'percent') {
  qp$bootstrap %>% group_by_(c(qp$groups, 'paran')) %>%
    do(one_paraci(., ci, method))
}
