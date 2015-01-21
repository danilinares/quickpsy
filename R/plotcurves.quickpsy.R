#' plotcurves.quickpsy
#'
#' @export
plotcurves.quickpsy <- function(qp, xpanel = NULL, ypanel = NULL, color = NULL,
                                averages = T, curves = T, thresholds = T) {
  if (!('curves' %in% names(qp))) stop('quickpsy should be called with plotcurves = TRUE')

  p <- ggplot()
  groups <- qp$groups
  ngroup <- length(groups)

  if (ngroup == 1) {
      color <- groups[[1]]
  }
  if (ngroup == 2) {
    if (is.null(xpanel) && is.null(ypanel) && is.null(color)) {
      xpanel <- groups[[1]]
      color <- groups[[2]]
      p <- p + facet_wrap(as.formula(paste0('~',xpanel)))
    }
    if (is.null(xpanel) && is.null(ypanel) && !is.null(color)) {
      remaining_var <- setdiff(groups, color)
      xpanel <- remaining_var
      p <- p + facet_wrap(as.formula(paste0('~',xpanel)))
    }
    if (!is.null(xpanel) && is.null(ypanel) && is.null(color)) {
      remaining_var <- setdiff(groups, xpanel)
      color <- remaining_var
      p <- p + facet_grid(as.formula(paste0('.~',xpanel)))
    }
    if (is.null(xpanel) && !is.null(ypanel) && is.null(color)) {
      remaining_var <- setdiff(groups, ypanel)
      color <- remaining_var
      p <- p + facet_grid(as.formula(paste0(ypanel,'~.')))
    }
    if (!is.null(xpanel) && is.null(ypanel) && !is.null(color)) {
      p <- p + facet_grid(as.formula(paste0('.~',xpanel)))
    }
    if (is.null(xpanel) && !is.null(ypanel) && !is.null(color)) {
      p <- p + facet_grid(as.formula(paste0(ypanel,'~.')))
    }
  }
  if (ngroup == 3) {
    if (is.null(xpanel) && is.null(ypanel) && is.null(color)) {
      xpanel <- groups[[1]]
      ypanel <- groups[[2]]
      color <- groups[[3]]
    }
    if (!is.null(xpanel) && is.null(ypanel) && is.null(color)) {
      remaining_var <- setdiff(groups, xpanel)
      ypanel <- remaining_var[[1]]
      color <- remaining_var[[2]]
    }
    if (is.null(xpanel) && !is.null(ypanel) && is.null(color)) {
      remaining_var <- setdiff(groups, ypanel)
      xpanel <- remaining_var[[1]]
      color <- remaining_var[[2]]
    }
    if (is.null(xpanel) && is.null(ypanel) && !is.null(color)) {
      remaining_var <- setdiff(groups, color)
      xpanel <- remaining_var[[1]]
      ypanel <- remaining_var[[2]]
    }
    if (!is.null(xpanel) && !is.null(ypanel) && is.null(color)) {
      remaining_var <- setdiff(groups, c(xpanel, ypanel))
      print(remaining_var)
      color <- remaining_var[[1]]
    }
    if (!is.null(xpanel) && is.null(ypanel) && !is.null(color)) {
      remaining_var <- setdiff(groups, c(xpanel, color))
      ypanel <- remaining_var[[1]]
    }
    if (is.null(xpanel) && !is.null(ypanel) && !is.null(color)) {
      remaining_var <- setdiff(groups, c(ypanel, color))
      xpanel <- remaining_var[[1]]
    }

    p <- p + facet_grid(as.formula(paste0(ypanel,'~',xpanel)))
   }
  if (ngroup == 1 || ngroup ==2 || ngroup == 3) {
    qp$averages[[color]] <- factor(qp$averages[[color]])
    qp$curves[[color]] <- factor(qp$curves[[color]])
    qp$thresholds[[color]] <- factor(qp$thresholds[[color]])
  }

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
      geom_linerange(data = qp$thresholds, aes_string(x = 'thre', ymin = 0,
                                 ymax = qp$thresholds$prob, color = color))
    }
  p
}
