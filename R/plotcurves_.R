#' plotcurves_
#'
#' @export
plotcurves_ <- function(qp, panel = NULL, xpanel = NULL, ypanel = NULL,
                       color = NULL, averages = T, curves = T, thresholds = T,
                       ci = T) {

  if (!('thresholds' %in% names(qp))) thresholds <- F
  if (!('thresholdsci' %in% names(qp))) ci <- F

  if (is.logical(qp$guess)) qp$guess <- 0
  if (is.logical(qp$lapses)) qp$lapses <- 0

  p <- ggplot2::ggplot()
  groups <- qp$groups
  ngroup <- length(groups)

  if (ngroup == 0) {
    if (averages) p <- p + ggplot2::geom_point(data = qp$averages,
                           aes_string(x = qp$x, y = 'y'))
    if (curves) p <- p + ggplot2::geom_line(data = qp$curves,
                         aes_string(x = 'x', y = 'y'))
    if (thresholds) p <- p + ggplot2::geom_linerange(data = qp$thresholds,
                             aes_string(x = 'thre', ymin = qp$guess,
                                        ymax = qp$thresholds$prob))
    if (ci) p <- p + ggplot2::geom_errorbarh(data = qp$thresholdsci,
                     height = .03, aes_string(x = 'threinf', xmin = 'threinf',
                     xmax = 'thresup', y = qp$thresholds$prob))
  }

  if (ngroup == 1) {
    if (!is.null(color)) groups <- setdiff(groups, color)
    if (is.null(color)) {
      color <- groups[[1]]
      groups <- setdiff(groups,groups[[1]])
    }
  }
  if (ngroup == 2) { ###########################################################
   if (!is.null(color)) groups <- setdiff(groups, color)
   if (!is.null(xpanel)) groups <- setdiff(groups, xpanel)
   if (!is.null(ypanel)) groups <- setdiff(groups, ypanel)
   if (!is.null(panel)) groups <- setdiff(groups, panel)

   if (is.null(color) && length(groups) == 1) {
     color <- groups[[1]]
     groups <- setdiff(groups,groups[[1]])
   }
   if (is.null(xpanel) && is.null(ypanel) && is.null(panel)) {
     panel <- groups[[1]]
     p <- p + ggplot2::facet_wrap(as.formula(paste0('~',panel)))
   }
   else {
     if (!is.null(panel)) {
       p <- p + ggplot2::facet_wrap(as.formula(paste0('~',panel)))
     }
     if (!is.null(xpanel)) {
       p <- p + ggplot2::facet_grid(as.formula(paste0('.~',xpanel)))
     }
     if (!is.null(ypanel)) {
       p <- p + ggplot2::facet_grid(as.formula(paste0(ypanel,'~.')))
     }
     if (!is.null(xpanel) && !is.null(ypanel)) {
       p <- p + ggplot2::facet_grid(as.formula(paste0(ypanel,'~', xpanel)))
     }
   }
  } ### end of  if (ngroup == 2) ###############################################

  if (ngroup == 3) { ###########################################################
   if (!is.null(color)) groups <- setdiff(groups, color)
   if (!is.null(xpanel)) groups <- setdiff(groups, xpanel)
   if (!is.null(ypanel)) groups <- setdiff(groups, ypanel)
   if (is.null(color)) {
     color <- groups[[1]]
     groups <- setdiff(groups,groups[[1]])
   }
   if (is.null(xpanel)) {
     xpanel <- groups[[1]]
     groups <- setdiff(groups,groups[[1]])
   }
   if (is.null(ypanel)) {
     ypanel <- groups[[1]]
     groups <- setdiff(groups,groups[[1]])
   }

   p <- p + ggplot2::facet_grid(as.formula(paste0(ypanel,'~',xpanel)))
  } ### end of  if (ngroup == 3) ###############################################

  if (ngroup == 1 || ngroup ==2 || ngroup == 3) {
    if (!is.null(color)) {
      qp$averages[[color]] <- factor(qp$averages[[color]])
      qp$curves[[color]] <- factor(qp$curves[[color]])
      qp$thresholds[[color]] <- factor(qp$thresholds[[color]])
      qp$thresholdsci[[color]] <- factor(qp$thresholdsci[[color]])

      if (averages) p <- p + ggplot2::geom_point(data = qp$averages,
                             aes_string(x = qp$x, y = 'y', color = color))

      if (curves) p <- p + ggplot2::geom_line(data = qp$curves,
                           aes_string(x = 'x', y = 'y', color = color))

      if (thresholds) p <- p + ggplot2::geom_linerange(data = qp$thresholds,
                               aes_string(x = 'thre', ymin = qp$guess,
                               ymax = qp$thresholds$prob, color = color))

      if (ci) p <- p + ggplot2::geom_errorbarh(data = qp$thresholdsci,
                       height = .03, aes_string(x = 'threinf', xmin = 'threinf',
                       color = color, xmax = 'thresup', y = qp$thresholds$prob))
    }
    else {
      if (averages) p <- p + ggplot2::geom_point(data = qp$averages,
                             aes_string(x = qp$x, y = 'y'))

      if (curves) p <- p + ggplot2::geom_line(data = qp$curves,
                           aes_string(x = 'x', y = 'y'))

      if (thresholds) p <- p + ggplot2::geom_linerange(data = qp$thresholds,
                               aes_string(x = 'thre', ymin = qp$guess,
                               ymax = qp$thresholds$prob))

      if (ci) p <- p + ggplot2::geom_errorbarh(data = qp$thresholdsci,
                       height = .03, aes_string(x = 'threinf', xmin = 'threinf',
                       xmax = 'thresup', y = qp$thresholds$prob))
    }
  }
  p
}
