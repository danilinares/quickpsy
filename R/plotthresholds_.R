#' plotthresholds_
#'
#' @export
plotthresholds_ <- function(qp, x = NULL, panel = NULL, xpanel = NULL,
                           ypanel = NULL, color = NULL, geom = 'bar', ci = T) {

  if (!('thresholds' %in% names(qp)))
    stop('To plot the thresholds, quickpsy should be called with thresholds = TRUE', call. = F)

  if (!('thresholdsci' %in% names(qp))) ci <- F

  p <- ggplot2::ggplot() + ggplot2::ylab(qp$x)

  groups <- qp$groups
  ngroup <- length(groups)

  if (ngroup == 0) { ###########################################################
    if (geom == 'bar') {
      p <- p + ggplot2::geom_bar(data = qp$thresholds,
               aes_string(x = 0, y = 'thre'), fill = 'grey',
               stat = 'identity', position = 'dodge') +
               ggplot2::theme(axis.title.x = element_blank(),
               axis.text.x = element_blank())
      if (ci) p <- p + ggplot2::geom_errorbar(data = qp$thresholdsci,
                       aes_string(x = 0, ymin = 'threinf', ymax = 'thresup'),
                       stat = 'identity', position = 'dodge', width = .5)
    }

    if (geom == 'point') {
      p <- p + ggplot2::geom_point(data = qp$thresholds,
               aes_string(x = 0, y = 'thre')) +
               ggplot2::theme(axis.title.x = element_blank(),
               axis.text.x = element_blank())
      if (ci) p <- p + ggplot2::geom_linerange(data = qp$thresholdsci,
                      aes_string(x = 0, ymin = 'threinf', ymax = 'thresup'),
                      stat = 'identity', position = 'dodge', width = .5)
    }
  }

  if (ngroup == 1) { ###########################################################
    if (is.null(color) && is.null(x)) color <- groups[[1]]

    if (!is.null(color)) {
      if (geom == 'bar') {
        qp$thresholds[[color]] <- factor(qp$thresholds[[color]])
        p <- p + ggplot2::geom_bar(data = qp$thresholds,
                   aes_string(x = color,fill = color, y = 'thre'),
                   stat = 'identity', position = 'dodge')
        if (ci) {
          qp$thresholdsci[[color]] <- factor(qp$thresholdsci[[color]])
          p <- p + ggplot2::geom_errorbar(data = qp$thresholdsci,
                         aes_string(x = color, ymin = 'threinf',
                         ymax = 'thresup'), stat = 'identity',
                         position = 'dodge', width = .5)
        }

      }
      if (geom == 'point') {
        p <- p + ggplot2::geom_point(data = qp$thresholds,
                          aes_string(x = color,color = color, y = 'thre'))
        if (ci) p <- p + ggplot2::geom_linerange(data = qp$thresholdsci,
                         aes_string(x = color, color = color, ymin = 'threinf',
                         ymax = 'thresup'), stat = 'identity', position = 'dodge')
      }
    }

    if (is.null(color) && !is.null(x)) {
      if (geom == 'bar') {
        p <- p + ggplot2::geom_bar(data = qp$thresholds, fill ='grey',
                          aes_string(x = x, y = 'thre'),
                          stat = 'identity', position = 'dodge')
        if (ci) p <- p + ggplot2::geom_errorbar(data = qp$thresholdsci,
                         aes_string(x = x, ymin = 'threinf', ymax = 'thresup'),
                         stat = 'identity', position = 'dodge', width = .5)
      }

      if (geom == 'point') {
        p <- p + ggplot2::geom_point(data = qp$thresholds, fill ='grey',
                 aes_string(x = x, y = 'thre')) +
                 ggplot2::geom_line(data = qp$thresholds, fill ='grey',
                 aes_string(x = x, y = 'thre'))
        if (ci) p <- p + ggplot2::geom_linerange(data = qp$thresholdsci,
                         aes_string(x = x, ymin = 'threinf', ymax = 'thresup'),
                         stat = 'identity', position = 'dodge')
      }
    }
  }

  if (ngroup == 2) { ###########################################################
    if (!is.null(x)) groups <- setdiff(groups, x)
    if (!is.null(color)) groups <- setdiff(groups, color)

    if (is.null(x)) {
      x <- groups[[1]]
      groups <- setdiff(groups, groups[[1]])
    }
    if (is.null(color)) color <- groups[[1]]
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
        p <- p + facet_grid(as.formula(paste0(ypanel,'~.')))
      }
    }
  }

  if (ngroup == 2 || ngroup == 3) {
    qp$thresholds[[color]] <- factor(qp$thresholds[[color]])

    if (geom == 'bar') {
      qp$thresholds[[x]] <- factor(qp$thresholds[[x]])
      p <- p + ggplot2::geom_bar(data = qp$thresholds,
               aes_string(x = x, fill = color, y = 'thre'),
               stat = 'identity', position = 'dodge')
      if (ci) {
        qp$thresholdsci[[color]] <- factor(qp$thresholdsci[[color]])
        qp$thresholdsci[[x]] <- factor(qp$thresholdsci[[x]])
        p <- p + ggplot2::geom_errorbar(data = qp$thresholdsci, width =.5,
                 aes_string(x = x, fill = color, ymin = 'threinf',
                 ymax = 'thresup'), stat = 'identity',
                 position = position_dodge(.9))
      }
    }
    if (geom == 'point') {
      p <- p + ggplot2::geom_point(data = qp$thresholds,
               aes_string(x = x, color = color, y = 'thre')) +
               ggplot2::geom_line(data = qp$thresholds,
              aes_string(x = x, color = color, group = color, y = 'thre'))
      if (ci) {
        qp$thresholdsci[[color]] <- factor(qp$thresholdsci[[color]])
        p <- p + ggplot2::geom_linerange(data = qp$thresholdsci,
                 aes_string(x = x, color = color, group = color,
                ymin = 'threinf', ymax = 'thresup'))
      }
    }
  }
  p
}
