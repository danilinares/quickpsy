#' curve_psy
#'
#' @export
curve_psy <- function(l, xmin, xmax, log) {
  f <- l$fit[[1]]
  if (is.null(xmin)) xmin <- min(f$d[[f$x]])
  else if (log) xmin <- log(xmin)

  if (is.null(xmax)) xmax <- max(f$d[[f$x]])
  else if (log) xmax <- log(xmax)


  xseq <- seq(xmin, xmax, length = 100)
  yseq <- f$psy_fun(xseq, f$para)
  if (log) xseq <- exp(xseq)
  out<- data.frame(x = xseq, y= yseq)
  names(out) <- c(f$x,'y')
  out
}
