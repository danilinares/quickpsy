#' Print quickpsy objects
#' @export
print.quickpsy <- function(x,...)
{
  cat("par\n")
  print(x$par)
  if ("thresholds" %in% names(x)) {
    cat("thresholds\n")
    print(x$thresholds)
  }
}
