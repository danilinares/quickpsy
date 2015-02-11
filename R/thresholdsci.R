#' thresholdsci
#'
#' @export
thresholdsci <- function(qp, ci = .95, method = 'percent') {
  if (length(qp$groups) == 0)
    one_thresholdsci(qp$thresholdsbootstrap, ci, method)
  else
    qp$thresholdsbootstrap %>% group_by_(qp$groups) %>%
      do(one_thresholdsci(., ci, method))
}

