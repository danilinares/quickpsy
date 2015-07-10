#' @export
summary.quickpsy <- function(x,...)
{
  print(x$par)
  print(x$parci)
  print(x$sse)
}
