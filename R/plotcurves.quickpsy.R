#' plotcurves.quickpsy
#'
#' @export
plotcurves.quickpsy <- function(l, xpanel, ypanel, color) {
  if (!('curves' %in% names(fit))) stop('quickpsy should be called with curv = TRUE')

  p <- ggplot()

  ngroup <- length(l$grouping_var)
  if (ngroup == 1) {
    color <- l$grouping_var[1]
  }
  if (ngroup == 2) {
    xpanel <- l$grouping_var[1]
    color <- l$grouping_var[2]
    p <- p + facet_wrap(as.formula(paste0('~',xpanel)))
  }
  if (ngroup == 3) {
    xpanel <- l$grouping_var[1]
    ypanel <- l$grouping_var[2]
    color <- l$grouping_var[3]
    p <- p + facet_grid(as.formula(paste0(xpanel,'~',ypanel)))
  }
  if (ngroup == 1 || ngroup ==2 || ngroup == 3) {
    p <- p +
      geom_point(data = l$d, aes_string(x = l$x, y = 'y', color = color)) +
      geom_line(data = l$curves, aes_string(x = l$x, y = 'y', color = color)) +
      geom_linerange(data = l$thresholds,
                     aes_string(x = 'thre', ymin = l$guess,
                                ymax = l$threprob, color = color))
  }
  p
}
