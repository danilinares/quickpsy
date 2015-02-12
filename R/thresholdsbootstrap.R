#' thresholdsbootstrap
#'
#' @export
thresholdsbootstrap <- function(qp, prob = NULL, log = F) {
  if (length(qp$groups) == 0)
    qp$parabootstrap %>% group_by_('sample') %>%
      do(one_threshold(., prob, log, qp$funname, qp$guess, qp$lapses))
  else
    qp$parabootstrap %>% group_by_(.dots = c(qp$groups, 'sample')) %>%
      do(one_threshold(., prob, log, qp$funname, qp$guess, qp$lapses))
}



