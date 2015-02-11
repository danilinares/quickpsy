#' plotcurves_
#'
#' @export
plotcurves_ <- function(qp, panel = NULL, xpanel = NULL, ypanel = NULL,
                       color = NULL, averages = T, curves = T, thresholds = T) {
  if (!('curves' %in% names(qp))) stop('To plot the curves, quickpsy should be called with curves = TRUE')

  p <- ggplot()
  groups <- qp$groups
  ngroup <- length(groups)

  if (ngroup == 0) {
    if (averages) {
      p <- p + geom_point(data = qp$averages, aes_string(x = qp$x, y = 'y'))
    }
    if (curves) {
      p <- p + geom_line(data = qp$curves, aes_string(x = 'x', y = 'y'))
    }
    if (thresholds) {
      p <- p +
        geom_linerange(data = qp$thresholds, aes_string(x = 'thre',
                       ymin = qp$guess, ymax = qp$thresholds$prob))
    }
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

   if (is.null(color)) {
     color <- groups[[1]]
     groups <- setdiff(groups,groups[[1]])
   }
   if (is.null(xpanel) && is.null(ypanel) && is.null(panel)) {
     panel <- groups[[1]]
     p <- p + facet_wrap(as.formula(paste0('~',panel)))
   }
   else {
     if (!is.null(panel)) {
       p <- p + facet_wrap(as.formula(paste0('~',panel)))
     }
     if (!is.null(xpanel)) {
       p <- p + facet_grid(as.formula(paste0('.~',xpanel)))
     }
     if (!is.null(ypanel)) {
       p <- p + facet_grid(as.formula(paste0(ypanel,'~.')))
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

   p <- p + facet_grid(as.formula(paste0(ypanel,'~',xpanel)))
  } ### end of  if (ngroup == 3) ###############################################

  if (ngroup == 1 || ngroup ==2 || ngroup == 3) {
    qp$averages[[color]] <- factor(qp$averages[[color]])
    qp$curves[[color]] <- factor(qp$curves[[color]])
    qp$thresholds[[color]] <- factor(qp$thresholds[[color]])


  if (averages) {
    p <- p + geom_point(data = qp$averages,
                        aes_string(x = qp$x, y = 'y', color = color))
  }
  if (curves) {
    p <- p + geom_line(data = qp$curves,
                       aes_string(x = 'x', y = 'y', color = color))
  }
  if (thresholds) {
    p <- p +
      geom_linerange(data = qp$thresholds, aes_string(x = 'thre',
                     ymin = qp$guess, ymax = qp$thresholds$prob, color = color))
    }
  }
  p
}
