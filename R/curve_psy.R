#' curve_psy
#'
#' @export
curve_psy <- function(l, xmin, xmax) {
  if (missing(xmin)) xmin <- min(l$d[[x]])
  if (missing(xmax)) xmax <- max(l$d[[x]])
  xseq <- seq(xmin, xmax, length = 100)
  yseq <- l$psy_fun(xseq, l$para)
  data.frame(x = xseq, y= yseq)
}
