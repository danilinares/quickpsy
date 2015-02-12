#' plotpara_
#'
#' @export
plotpara_ <- function(qp, x = NULL, panel = NULL, xpanel = NULL,
                           ypanel = NULL, color = NULL, geom = 'bar') {

  p <- ggplot() + ylab(qp$x)

  groups <- qp$groups
  ngroup <- length(groups)

  if (ngroup != 3) p <- p + facet_wrap(~paran)

  if (ngroup == 0) {

    if (geom == 'bar') {
      p <- p + geom_bar(data = qp$para,
                        aes_string(x = 0, y = 'para'), fill = 'grey',
                        stat = 'identity', position = 'dodge') +
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank())
      if ('parabootstrap' %in% names(qp)) {
        p <- p + geom_errorbar(data = qp$paraci, width = .5,
                                aes_string(x = 0,
                                           ymin = 'parainf', ymax = 'parasup'),
                                stat = 'identity', position = 'dodge')
      }
    }
    if (geom == 'point') {
      p <- p + geom_point(data = qp$para,
                        aes_string(x = 0, y = 'para')) +
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank())
      if ('parabootstrap' %in% names(qp)) {
        p <- p + geom_linerange(data = qp$paraci,
                                aes_string(x = 0,
                                           ymin = 'parainf', ymax = 'parasup'),
                                stat = 'identity', position = 'dodge')
      }
    }

  }

  if (ngroup == 1) {
    if (is.null(color) && is.null(x)) color <- groups[[1]]

    if (!is.null(color)) {
      qp$para[[color]] <- factor(qp$para[[color]])
      if (geom == 'bar') {
        p <- p + geom_bar(data = qp$para,
                   aes_string(x = color,fill = color, y = 'para'),
                   stat = 'identity', position = 'dodge')
        if ('parabootstrap' %in% names(qp)) {
          p <- p + geom_errorbar(data = qp$paraci,
                                 aes_string(x = color,
                                            ymin = 'parainf', ymax = 'parasup'),
                                 stat = 'identity', position = 'dodge', width = .5)
        }
      }
      if (geom == 'point') {
        p <- p + geom_point(data = qp$para,
                          aes_string(x = color, color = color, y = 'para'))
        if ('parabootstrap' %in% names(qp)) {
          qp$paraci[[color]] <- factor(qp$paraci[[color]])
          p <- p + geom_linerange(data = qp$paraci,
                                 aes_string(x = color, color = color,
                                            ymin = 'parainf', ymax = 'parasup'),
                                 stat = 'identity', position = 'dodge')
        }
      }

    }
    if (is.null(color) && !is.null(x)) {
      if (geom == 'bar') {
        p <- p + geom_bar(data = qp$para,
                          aes_string(x = x, y = 'para'), fill = 'grey',
                          stat = 'identity', position = 'dodge')
        if ('parabootstrap' %in% names(qp)) {
          p <- p + geom_errorbar(data = qp$paraci,
                                  aes_string(x = x, width = .5,
                                             ymin = 'parainf', ymax = 'parasup'),
                                  stat = 'identity', position = 'dodge')
        }
      }
      if (geom == 'point') {
        p <- p + geom_point(data = qp$para,
                          aes_string(x = x, y = 'para')) +
          geom_line(data = qp$para,
                     aes_string(x = x, y = 'para'))
        if ('parabootstrap' %in% names(qp)) {
          p <- p + geom_linerange(data = qp$paraci,
                                  aes_string(x = x,
                                             ymin = 'parainf', ymax = 'parasup'),
                                  stat = 'identity', position = 'dodge')
        }
      }

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
      p <- p + facet_grid(as.formula(paste0(panel, '~paran')))
    }
    else {
      if (!is.null(xpanel)) {
        p <- p + facet_grid(as.formula(paste0('paran~',xpanel)))
      }
      if (!is.null(ypanel)) {
        p <- p + facet_grid(as.formula(paste0(ypanel,'~paran')))
      }
    }

  } ### end of if (ngroup == 3) ################################################

  if (ngroup == 2 || ngroup == 3) {
    qp$para[[color]] <- factor(qp$para[[color]])

    if (geom == 'bar') {
      qp$para[[x]] <- factor(qp$para[[x]])
      p <- p + geom_bar(data = qp$para,
               aes_string(x = x, fill = color, y = 'para'),
               stat = 'identity', position = 'dodge')
      if ('parabootstrap' %in% names(qp)) {
        qp$paraci[[color]] <- factor(qp$paraci[[color]])
        #qp$paraci[[x]] <- factor(qp$paraci[[x]])
        p <- p + geom_errorbar(data = qp$paraci, width = .5,
                      aes_string(x = x, fill = color,
                                 ymin = 'parainf', ymax = 'parasup'),
                                 stat = 'identity',
                      position = position_dodge(width=0.9))
      }
    }
    if (geom == 'point') {
      p <- p + geom_point(data = qp$para,
                        aes_string(x = x, color = color, y = 'para')) +
               geom_line(data = qp$para,
                     aes_string(x = x, color = color, y = 'para', group =color))
      if ('parabootstrap' %in% names(qp)) {
        qp$paraci[[color]] <- factor(qp$paraci[[color]])
        #qp$paraci[[x]] <- factor(qp$paraci[[x]])
        p <- p + geom_linerange(data = qp$paraci,
                               aes_string(x = x, color = color,
                                          ymin = 'parainf', ymax = 'parasup'))
      }
    }

  }
  p
}
