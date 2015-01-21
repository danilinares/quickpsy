#' plotthresholds
#'
#' @export
plotthresholds <- function(qp, x = NULL, panel = NULL, xpanel = NULL,
                           ypanel = NULL, color = NULL, geom = 'bar') {

  if (!missing(x)) x <- deparse(substitute(x))
  if (!missing(panel)) panel <- deparse(substitute(panel))
  if (!missing(xpanel)) xpanel <- deparse(substitute(xpanel))
  if (!missing(ypanel)) ypanel <- deparse(substitute(ypanel))
  if (!missing(color)) color <- deparse(substitute(color))

 plotthresholds_(qp, x, panel, xpanel, ypanel, color, geom)
}
