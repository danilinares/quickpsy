#' Plot the thresholds
#'
#' \code{plotthresholds} plot the thresholds.
#' @param qp output from quickpsy.
#' @param x Name of the variable to displayed in the x-axis.
#' @param panel Name of the variable to be split in panels.
#' @param xpanel Name of the variable to be split in horizontal panels.
#' @param ypanel Name of the variable to be split in vertical panels.
#' @param color Name of the variable codded by color.
#' @param geom If \code{'bar'} displays bars.
#' @param sizeerrorbar Line width of the error bars.
#' If \code{'point'} displays points (default is 'bar').
#' @param ci If \code{FALSE} confidence intervals are not plotted
#' (default is \code{TRUE}).
#' @examples
##' fit <- quickpsy(qpdat, phase, resp, grouping = c("participant", "cond"), bootstrap = "none")
#' plotthresholds(fit)
#' @export
#' @importFrom rlang .data
plotthresholds <- function(qp, x = NULL, panel = NULL, xpanel = NULL,
                           ypanel = NULL, color = NULL, geom = "bar", ci = T,
                           sizeerrorbar = .5) {

  if (!missing(x)) x <- deparse(substitute(x))
  if (!missing(panel)) panel <- deparse(substitute(panel))
  if (!missing(xpanel)) xpanel <- deparse(substitute(xpanel))
  if (!missing(ypanel)) ypanel <- deparse(substitute(ypanel))
  if (!missing(color)) color <- deparse(substitute(color))

  if (!("thresholds" %in% names(qp)))
    stop('To plot the thresholds, quickpsy should be called with thresholds = TRUE', call. = FALSE)

  if (!("threinf" %in% names(qp$thresholds))) ci <- FALSE

  p <- ggplot()
  if (qp$log) p <- p + ylab(paste0("log(", qp$x_str, ")"))
  else p <- p + ylab(qp$x_str)

  grouping <- qp$grouping
  ngrouping <- length(grouping)

  if (ngrouping == 0) { ###########################################################
    if (geom == "bar") {
      p <- p + geom_bar(data = qp$thresholds, aes(x = 0, y = .data$thre), fill = "grey",
                        stat = "identity", position = "dodge") +
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank())
      if (ci) p <- p + geom_errorbar(data = qp$thresholds,
                                     aes(x = 0, ymin = .data$threinf, ymax = .data$thresup),
                                     stat = "identity", position = position_dodge(0.9), width = .5,
                                     size=sizeerrorbar)
    }

    if (geom == "point") {
      p <- p + geom_point(data = qp$thresholds, aes(x = 0, y = .data$thre)) +
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank())
      if (ci) p <- p + geom_linerange(data = qp$thresholds,
                                      aes(x = 0, ymin = .data$threinf, ymax = .data$thresup),
                                      stat = "identity", position = position_dodge(0.9), width = .5)
    }
  }

  if (ngrouping == 1) { ###########################################################
    if (is.null(color) && is.null(x)) color <- grouping[[1]]

    if (!is.null(color)) {
      if (geom == "bar") {
        qp$thresholds[[color]] <- factor(qp$thresholds[[color]])
        p <- p + geom_bar(data = qp$thresholds,
                          aes(x = .data[[color]], fill = .data[[color]], y = .data$thre),
                          stat = 'identity', position = position_dodge(0.9))
        if (ci) {
          p <- p + geom_errorbar(data = qp$thresholds,
                                 aes(x = .data[[color]], ymin = .data$threinf,
                                            ymax = .data$thresup), stat = 'identity',
                                 position = position_dodge(0.9), width = .5,
                                 size=sizeerrorbar)
        }

      }
      if (geom == 'point') {
        p <- p + geom_point(data = qp$thresholds,
                            aes(x = .data[[color]], color = .data[[color]], y = .data$thre))
        if (ci) p <- p + geom_linerange(data = qp$thresholds,
                                        aes(x = .data[[color]], color = .data[[color]], ymin = .data$threinf,
                                                   ymax = .data$thresup), stat = 'identity', position = position_dodge(0.9))
      }
    }

    if (is.null(color) && !is.null(x)) {
      if (geom == 'bar') {
        p <- p + geom_bar(data = qp$thresholds, fill ='grey',
                          aes(x = x, y = .data$thre),
                          stat = 'identity', position = position_dodge(0.9))
        if (ci) p <- p + geom_errorbar(data = qp$thresholds,
                                       aes(x = x, ymin = .data$threinf, ymax = .data$thresup),
                                       stat = 'identity', position = position_dodge(0.9), width = .5,
                                       size=sizeerrorbar)
      }

      if (geom == 'point') {
        p <- p + geom_point(data = qp$thresholds, fill ='grey',
                            aes(x = x, y = .data$thre)) +
          geom_line(data = qp$thresholds, fill ='grey',
                    aes(x = x, y = .data$thre))
        if (ci) p <- p + geom_linerange(data = qp$thresholds,
                                        aes(x = x, ymin = .data$threinf, ymax = .data$thresup),
                                        stat = 'identity', position = position_dodge(0.9))
      }
    }
  }

  if (ngrouping == 2) { ###########################################################
    if (!is.null(x)) grouping <- setdiff(grouping, x)
    if (!is.null(color)) grouping <- setdiff(grouping, color)
    if (!is.null(xpanel)) grouping <- setdiff(grouping, xpanel)
    if (!is.null(ypanel)) grouping <- setdiff(grouping, ypanel)
    if (!is.null(panel)) grouping <- setdiff(grouping, panel)

    if (is.null(x)) {
      x <- grouping[[1]]
      grouping <- setdiff(grouping, grouping[[1]])
    }

    if (is.null(xpanel) && is.null(ypanel) && is.null(panel)) {
      if (is.null(color)) color <- grouping[[1]]
      qp$thresholds[[color]] <- factor(qp$thresholds[[color]])
      if (geom == 'bar') {

        qp$thresholds[[x]] <- factor(qp$thresholds[[x]])
        p <- p + geom_bar(data = qp$thresholds,
                          aes(x = .data[[x]], fill = .data[[color]], y = .data$thre),
                          stat = 'identity', position = position_dodge(0.9))
        if (ci) {
          p <- p + geom_errorbar(data = qp$thresholds, width =.5,
                                 aes(x = .data[[x]], group = .data[[color]], ymin = .data$threinf,
                                            ymax = .data$thresup), stat = 'identity',
                                 size=sizeerrorbar,
                                 position = position_dodge(.9))
        }
      }
      if (geom == 'point') {
        p <- p + geom_point(data = qp$thresholds,
                            aes(x = .data[[x]], color = .data[[color]], y = .data$thre))
        if (ci) {
          p <- p + geom_linerange(data = qp$thresholds,
                                  aes(x = .data[[x]], color = .data[[color]], group = .data[[color]],
                                             ymin = .data$threinf, ymax = .data$thresup))
        }
      }
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

      if (geom == 'bar') {
        qp$thresholds[[x]] <- factor(qp$thresholds[[x]])
        p <- p + geom_bar(data = qp$thresholds,
                          aes(x = .data[[x]], y = .data$thre),
                          stat = 'identity', position = position_dodge(0.9))
        if (ci) {
          p <- p + geom_errorbar(data = qp$thresholds, width =.5,
                                 aes(x = .data[[x]], ymin = .data$threinf,
                                            ymax = .data$thresup), stat = 'identity',
                                 size=sizeerrorbar,
                                 position = position_dodge(.9))
        }
      }
      if (geom == 'point') {
        p <- p + geom_point(data = qp$thresholds,
                            aes(x = .data[[x]], y = .data$thre)) +
          geom_line(data = qp$thresholds,
                    aes(x = x, y = .data$thre))
        if (ci) {
          p <- p + geom_linerange(data = qp$thresholds,
                                  aes(x = .data[[x]],
                                             ymin = .data$threinf, ymax = .data$thresup))
        }
      }
    }
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

    if (geom == 'bar') {
      qp$thresholds[[x]] <- factor(qp$thresholds[[x]])
      p <- p + geom_bar(data = qp$thresholds,
                        aes(x = .data[[x]], fill = .data[[color]], y = .data$thre),
                        stat = 'identity', position = 'dodge')
      if (ci) {
        p <- p + geom_errorbar(data = qp$thresholds, width =.5,
                               aes(x = .data[[x]], group = .data[[color]], ymin = .data$threinf,
                                          ymax = .data$thresup), stat = 'identity',
                               size=sizeerrorbar,
                               position = position_dodge(.9))
      }
    }
    if (geom == 'point') {
      p <- p + geom_point(data = qp$thresholds,
                          aes(x = .data[[x]], color = .data[[color]], y = .data$thre)) +
        geom_line(data = qp$thresholds,
                  aes(x = , color = .data[[color]], group = .data[[color]], y = .data$thre))
      if (ci) {
        p <- p + geom_linerange(data = qp$thresholds,
                                aes(x = .data[[x]], color = .data[[color]], group = .data[[color]],
                                           ymin = .data$threinf, ymax = .data$thresup))
      }
    }

  }


  p
}
