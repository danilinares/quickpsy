#' plotthresholds_
#'
#' @export
plotthresholds_ <- function(qp, x = NULL, panel = NULL, xpanel = NULL,
                           ypanel = NULL, color = NULL, geom = 'bar') {

  if (!('thresholds' %in% names(qp))) stop('To plot the thresholds, quickpsy should be called with thresholds = TRUE')

  p <- ggplot() + ylab(qp$x)

  groups <- qp$groups
  ngroup <- length(groups)

  if (ngroup == 0) {

    p <- p + geom_bar(data = qp$thresholds,
                      aes_string(x = 0, y = 'thre'),
                      stat = 'identity', position = 'dodge') +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank())
  }

  if (ngroup == 1) {
    if (is.null(color) && is.null(x)) color <- groups[[1]]

    if (!is.null(color)) {
      qp$thresholds[[color]] <- factor(qp$thresholds[[color]])
      p <- p + geom_bar(data = qp$thresholds,
                 aes_string(x = color,fill = color, y = 'thre'),
                 stat = 'identity', position = 'dodge')
      }
    if (is.null(color) && !is.null(x)) {
      p <- p + geom_bar(data = qp$thresholds,
                        aes_string(x = x, y = 'thre'),
                        stat = 'identity', position = 'dodge')
    }
  }

  if (ngroup == 2) {
    if (!is.null(x)) groups <- setdiff(groups, x)
    if (!is.null(color)) groups <- setdiff(groups, color)

    if (is.null(x)) {
      x <- groups[[1]]
      groups <- setdiff(groups,groups[[1]])
    }
    if (is.null(color)) {
      color <- groups[[1]]
    }
    qp$thresholds[[color]] <- factor(qp$thresholds[[color]])
  }

  if (ngroup == 3) { ###########################################################
    if (!is.null(x)) groups <- setdiff(groups, x)
    if (!is.null(color)) groups <- setdiff(groups, color)
    if (!is.null(xpanel)) groups <- setdiff(groups, xpanel)
    if (!is.null(ypanel)) groups <- setdiff(groups, ypanel)
    if (!is.null(panel)) groups <- setdiff(groups, panel)

    if (is.null(x)) {
      x <- groups[[1]]
      groups <- setdiff(groups,groups[[1]])
    }
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

    qp$thresholds[[color]] <- factor(qp$thresholds[[color]])
  } ### end of if (ngroup == 3) ################################################

  if (ngroup == 2 || ngroup == 3) {
    if (geom == 'bar') {
    p <- p + geom_bar(data = qp$thresholds,
               aes_string(x = x, fill = color, y = 'thre'),
               stat = 'identity', position = 'dodge')
    }
    if (geom == 'point') {
      p <- p + geom_point(data = qp$thresholds,
                        aes_string(x = x, color = color, y = 'thre')) +
               geom_line(data = qp$thresholds,
                     aes_string(x = x, color = color, y = 'thre'))
    }
  }

  p
}
