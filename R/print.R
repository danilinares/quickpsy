#' @export
print.quickpsy <- function(qp)
{
  print(qp$para)
  print(qp$paraci)
  if ('thresholds' %in% names(qp)) print(qp$thresholds)
  if ('thresholdsci' %in% names(qp)) print(qp$thresholdsci)
}
