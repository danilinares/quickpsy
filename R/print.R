#' @export
print.quickpsy <- function(qp)
{
  print(qp$para)
  print(qp$paraci)
  print(qp$thresholds)
  print(qp$thresholdsci)
}
