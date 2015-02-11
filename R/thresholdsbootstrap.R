#' thresholdsbootstrap
#'
#' @export
thresholdsbootstrap <- function(qp, prob = NULL, log = F) {
  if (length(qp$groups) == 0)
    one_threshold(qp$bootstrap, prob, log, qp$funname, qp$guess, qp$lapses)
  else
    qp$bootstrap %>% group_by_(c(qp$groups, 'sample')) %>%
      do(one_threshold(., prob, log, qp$funname, qp$guess, qp$lapses))
}



