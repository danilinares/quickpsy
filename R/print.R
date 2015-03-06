#' @export
print.quickpsy <- function(qp)
{
  print(qp$par)
  print(qp$parci)
  if ('thresholds' %in% names(qp)) print(qp$thresholds)
  if ('thresholdsci' %in% names(qp)) print(qp$thresholdsci)
}
