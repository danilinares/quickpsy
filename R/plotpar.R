#' Plot the values of the parameters
#'
#' \code{plotpar} plot the values of the parameters.
#' @param qp output from quickpsy.
#' @param x Name of the variable to displayed in the x-axis.
#' @param panel Name of the variable to be split in panels.
#' @param xpanel Name of the variable to be split in horizontal panels.
#' @param ypanel Name of the variable to be split in vertical panels.
#' @param color Name of the variable codded by color.
#' @param geom If \code{'bar'} displays bars.
#' If \code{'point'} displays points (default is \code{'bar'}).
#' @param ci If \code{FALSE} confidence intervals are not plotted
#' (default is \code{TRUE}).
#' @examples
#' fit <- quickpsy(qpdat, phase, resp, grouping = c("participant", "cond"), bootstrap = "none")
#' plot(fit)
#' plot(fit, color = cond)
#' plotpar(fit)
#' plotthresholds(fit, geom = "point")
#' @export
#' @importFrom rlang .data
plotpar <- function(qp, x = NULL, panel = NULL, xpanel = NULL,
                           ypanel = NULL, color = NULL, geom = 'bar', ci = T) {

  if (!missing(x)) x <- deparse(substitute(x))
  if (!missing(panel)) panel <- deparse(substitute(panel))
  if (!missing(xpanel)) xpanel <- deparse(substitute(xpanel))
  if (!missing(ypanel)) ypanel <- deparse(substitute(ypanel))
  if (!missing(color)) color <- deparse(substitute(color))

  if (!("parinf" %in% names(qp$par))) ci <- FALSE


  p <- ggplot()

  grouping <- qp$grouping
  ngrouping <- length(grouping)


  if (ngrouping != 3) p <- p + facet_wrap(~parn, scales = 'free')

  if (ngrouping == 0) { ###########################################################

    if (geom == 'bar') {
      p <- p + geom_bar(data = qp$par,
                        aes(x = 0, y = .data$par), fill = 'grey',
                        stat = 'identity', position = position_dodge(0.9)) +
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank())
      if (ci) p <- p + geom_errorbar(data = qp$par, width = .5,
                                     aes(x = 0, ymin = .data$parinf, ymax = .data$parsup),
                                     stat = 'identity', position = position_dodge(0.9))
    }
    if (geom == 'point') {
      p <- p + geom_point(data = qp$par,
                          aes(x = 0, y = .data$par)) +
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank())
      if (ci) p <- p + geom_linerange(data = qp$par,
                                      aes(x = 0, ymin = .data$parinf, ymax = .data$parsup),
                                      stat = 'identity', position = position_dodge(0.9))
    }
  }

  if (ngrouping == 1) { ###########################################################
    if (is.null(color) && is.null(x)) color <- grouping[[1]]

    if (!is.null(color)) {
      if (geom == 'bar') {
        qp$par[[color]] <- factor(qp$par[[color]])
        p <- p + geom_bar(data = qp$par,
                          aes(x = .data[[color]], fill = .data[[color]], y = .data$par),
                          stat = 'identity', position = position_dodge(0.9))
        if (ci) {
          p <- p + geom_errorbar(data = qp$par,
                                 aes(x = .data[[color]], ymin = .data$parinf, ymax = .data$parsup),
                                 stat = 'identity', position = position_dodge(0.9), width = .5)
        }
      }
      if (geom == 'point') {
        p <- p + geom_point(data = qp$par,
                            aes(x = .data[[color]], color = .data[[color]], y = .data$par))
        if (ci) p <- p + geom_linerange(data = qp$par,
                                        aes(x = .data[[color]], color = .data[[color]], ymin = .data$parinf,
                                                   ymax = .data$parsup), stat = 'identity', position = position_dodge(0.9))
      }
    }
    if (is.null(color) && !is.null(x)) {
      if (geom == 'bar') {
        qp$par[[x]] <- factor(qp$par[[x]])
        p <- p + geom_bar(data = qp$par,
                          aes(x = x, y = .data$par), fill = 'grey',
                          stat = 'identity', position = position_dodge(0.9))
        if (ci) {
          p <- p + geom_errorbar(data = qp$par,
                                 aes(x = x, width = .5, ymin = .data$parinf,
                                            ymax = .data$parsup), stat = 'identity',
                                 position = position_dodge(0.9))
        }
      }
      if (geom == 'point') {
        p <- p + geom_point(data = qp$par,
                            aes(x = x, y = .data$par)) +
          geom_line(data = qp$par,
                    aes(x = x, y = .data$par))
        if (ci) p <- p + geom_linerange(data = qp$par,
                                        aes(x = x, ymin = .data$parinf, ymax = .data$parsup),
                                        stat = 'identity', position = position_dodge(0.9))
      }
    }
  }

  if (ngrouping == 2) { ###########################################################
    if (!is.null(x)) grouping <- setdiff(grouping, x)
    if (!is.null(color)) grouping <- setdiff(grouping, color)

    if (is.null(x)) {
      x <- grouping[[1]]
      grouping <- setdiff(grouping, grouping[[1]])
    }
    if (is.null(color)) color <- grouping[[1]]
  }

  if (ngrouping == 3) { ###########################################################
    if (!is.null(x)) grouping <- setdiff(grouping, x)
    if (!is.null(color)) grouping <- setdiff(grouping, color)
    if (!is.null(xpanel)) grouping <- setdiff(grouping, xpanel)
    if (!is.null(ypanel)) grouping <- setdiff(grouping, ypanel)
    if (!is.null(panel)) grouping <- setdiff(grouping, panel)

    if (is.null(x)) {
      x <- grouping[[1]]
      grouping <- setdiff(grouping,grouping[[1]])
    }
    if (is.null(color)) {
      color <- grouping[[1]]
      grouping <- setdiff(grouping,grouping[[1]])
    }
    if (is.null(xpanel) && is.null(ypanel) && is.null(panel)) {
      panel <- grouping[[1]]
      p <- p + facet_grid(as.formula(paste0(panel, '~parn')),
                          scales = 'free_y')
    }
    else {
      if (!is.null(xpanel)) p <- p +
          facet_grid(as.formula(paste0('parn~',xpanel)),
                     scales = 'free_y')

      if (!is.null(ypanel)) p <- p +
          facet_grid(as.formula(paste0(ypanel,'~parn')),
                     scales = 'free_y')
    }
  }

  if (ngrouping == 2 || ngrouping == 3) {
    qp$par[[color]] <- factor(qp$par[[color]])

    if (geom == 'bar') {
      qp$par[[x]] <- factor(qp$par[[x]])
      p <- p + geom_bar(data = qp$par,
                        aes(x = .data[[x]], fill = .data[[color]], y = .data$par),
                        stat = 'identity', position = position_dodge(0.9))
      if (ci) {
        p <- p + geom_errorbar(data = qp$par, width = .5,
                               aes(x = .data[[x]], group = .data[[color]], ymin = .data$parinf,
                                          ymax = .data$parsup), stat = 'identity',
                               position = position_dodge(width=0.9))
      }
    }

    if (geom == 'point') {
      p <- p + geom_point(data = qp$par,
                          aes(x = .data[[x]], color = .data[[color]], y = .data$par)) +
        geom_line(data = qp$par, aes(x = .data[[x]],
                                            color = .data[[color]], y = .data$par, group =.data[[color]]))
      if (ci) {
        p <- p + geom_linerange(data = qp$par,
                                aes(x = .data[[x]], color = .data[[color]],
                                           ymin = .data$parinf, ymax = .data$parsup))
      }
    }
  }
  p
}
