#' plotcurves
#'
#' @export
plotcurves <- function(qp, panel = NULL, xpanel = NULL, ypanel = NULL,
                       color = NULL, averages = T, curves = T, thresholds = T,
                       ci = T) {

  if (!missing(panel)) panel <- deparse(substitute(panel))
  if (!missing(xpanel)) xpanel <- deparse(substitute(xpanel))
  if (!missing(ypanel)) ypanel <- deparse(substitute(ypanel))
  if (!missing(color)) color <- deparse(substitute(color))

  plotcurves_(qp, panel, xpanel, ypanel, color, averages, curves,
              thresholds, ci)
}
