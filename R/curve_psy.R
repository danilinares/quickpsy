#' curve_psy
#'
#' @export
curve_psy <- function(l, xmin, xmax) {
  f <- l$fit[[1]]
  if (missing(xmin)) xmin <- min(f$d[[f$x]])
  if (missing(xmax)) xmax <- max(f$d[[f$x]])
  xseq <- seq(xmin, xmax, length = 100)
  yseq <- f$psy_fun(xseq, f$para)
  out<- data.frame(x = xseq, y= yseq)
  names(out) <- c(f$x,'y')
  out
}
