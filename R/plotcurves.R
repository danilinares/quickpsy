#' Plot the curves
#'
#' \code{plotcurves} plot the curves.
#' @param qp output from quickpsy
#' @param panel Name of the variable to be split in panels.
#' @param xpanel Name of the variable to be split in horizontal panels.
#' @param ypanel Name of the variable to be split in vertical panels.
#' @param color Name of the variable codded by color.
#' @param averages If \code{FALSE} averaged probabilities are not plotted
#' (default is \code{TRUE}).
#' @param curves If \code{FALSE} curves are not plotted
#' (default is \code{TRUE})
#' @param thresholds If \code{FALSE} thresholds  are not plotted
#' (default is \code{TRUE})
#' @param ci If \code{FALSE} confidence intervals are not plotted
#' (default is \code{TRUE})
#' @export
#' @importFrom stats as.formula
#' @importFrom rlang .data
#' @importFrom utils packageVersion
plotcurves <- function(qp, panel = NULL, xpanel = NULL, ypanel = NULL,
                       color = NULL,
                       averages = TRUE, curves = TRUE, thresholds = TRUE,
                       ci = TRUE) {

  if (!missing(panel)) panel <- deparse(substitute(panel))
  if (!missing(xpanel)) xpanel <- deparse(substitute(xpanel))
  if (!missing(ypanel)) ypanel <- deparse(substitute(ypanel))
  if (!missing(color)) color <- deparse(substitute(color))

  if (!("thresholds" %in% names(qp))) thresholds <- FALSE
  if (!("threinf" %in% names(qp$thresholds))) ci <- FALSE

  if (is.logical(qp$guess)) qp$guess <- 0
  if (is.logical(qp$lapses)) qp$lapses <- 0

  p <- ggplot()

  if (qp$log) {
    # xmin <- min(qp$averages[[qp$x_str]])
    # xmax <- max(qp$averages[[qp$x_str]])
    # breaks <- signif(exp( seq(log(xmin), log(xmax), length.out=4) ), digits = 2)
    # p <- p + scale_x_log10(breaks = breaks)
    p <- p + xlab(paste0("log(", qp$x_str, ")"))
  }

  grouping <- qp$grouping
  ngrouping <- length(grouping)

  if (ngrouping == 1) { ###########################################################
    if (!is.null(color)) grouping <- setdiff(grouping, color)
    if (!is.null(xpanel)) grouping <- setdiff(grouping, xpanel)
    if (!is.null(ypanel)) grouping <- setdiff(grouping, ypanel)
    if (!is.null(panel)) grouping <- setdiff(grouping, panel)
    if (length(grouping) == 1) color <- grouping[[1]]
    if (!is.null(panel)) p <- p +
        facet_wrap(as.formula(paste0('~',panel)))
    if (!is.null(xpanel)) p <- p +
        facet_grid(as.formula(paste0('.~',xpanel)))
    if (!is.null(ypanel)) p <- p +
        facet_grid(as.formula(paste0(ypanel,'~.')))
  }

  if (ngrouping == 2) { ###########################################################
    if (!is.null(color)) grouping <- setdiff(grouping, color)
    if (!is.null(xpanel)) grouping <- setdiff(grouping, xpanel)
    if (!is.null(ypanel)) grouping <- setdiff(grouping, ypanel)
    if (!is.null(panel)) grouping <- setdiff(grouping, panel)
    if (is.null(color) && length(grouping) >= 1) {
      color <- grouping[[1]]
      grouping <- setdiff(grouping,grouping[[1]])
    }

    if (is.null(xpanel) && is.null(ypanel) && is.null(panel)) {
      panel <- grouping[[1]]
      p <- p + facet_wrap(as.formula(paste0('~',panel)))
    }
    else {
      if (!is.null(panel)) p <- p +
          facet_wrap(as.formula(paste0('~',panel)))
      if (!is.null(xpanel)) p <- p +
          facet_grid(as.formula(paste0('.~',xpanel)))
      if (!is.null(ypanel)) p <- p +
          facet_grid(as.formula(paste0(ypanel,'~.')))
      if (!is.null(xpanel) && !is.null(ypanel)) p <- p +
          facet_grid(as.formula(paste0(ypanel,'~', xpanel)))
    }
  }

  if (ngrouping == 3) { ###########################################################
    if (!is.null(color)) grouping <- setdiff(grouping, color)
    if (!is.null(xpanel)) grouping <- setdiff(grouping, xpanel)
    if (!is.null(ypanel)) grouping <- setdiff(grouping, ypanel)
    if (is.null(color)) {
      color <- grouping[[1]]
      grouping <- setdiff(grouping,grouping[[1]])
    }
    if (is.null(xpanel)) {
      xpanel <- grouping[[1]]
      grouping <- setdiff(grouping,grouping[[1]])
    }
    if (is.null(ypanel)) {
      ypanel <- grouping[[1]]
    }

    p <- p + facet_grid(as.formula(paste0(ypanel,'~',xpanel)))
  }

  # ### plotting ###################################################################
  if (ngrouping == 0) {
    if (averages) p <- p + geom_point(data = qp$averages, aes(x = .data[[qp$x_str]], y = .data$prob))
    if (curves) p <- p + geom_line(data = qp$curves, aes(x = .data$x, y = .data$y))
    if (thresholds) p <- p + geom_linerange(data = qp$thresholds,
                                            aes(x = .data$thre, ymin = qp$guess, ymax = .data$prob))
    if (ci) p <- p + geom_errorbarh(data = qp$thresholds, height = .03,
                                    aes(xmin = .data$threinf, xmax = .data$thresup, y = .data$prob))
  }
  if (ngrouping == 1 || ngrouping ==2 || ngrouping == 3) {
    if (!is.null(color)) {
      qp$averages[[color]] <- factor(qp$averages[[color]])
      qp$curves[[color]] <- factor(qp$curves[[color]])

      if (averages) p <- p + geom_point(data = qp$averages, aes(x = .data[[qp$x_str]], y = .data$prob, color = .data[[color]]))

      if (curves) p <- p + geom_line(data = qp$curves, aes(x = .data$x, y = .data$y, color = .data[[color]]))

      if (thresholds) {

        qp$thresholds[[color]] <- factor(qp$thresholds[[color]])

        # get present axis limits
        if (packageVersion('ggplot2') >= '2.2.0')
          axisYrange <- ggplot_build(p)$layout$panel_ranges[[1]]$y.range

        else
          axisYrange <- ggplot_build(p)$panel$ranges[[1]]$y.range


        p <- p + geom_linerange(data = qp$thresholds,
                                aes(x = .data$thre,
                                           ymin = qp$guess, #axisYrange[1] - .2, #make sure extends below axis line
                                           ymax = .data$prob, color = .data[[color]]))
        #Because threshline extended below axis limit, axis automatically scaled below it.
        #Restore it to its former values
     #   p <- p + coord_cartesian(ylim = axisYrange)
      }
      if (ci) {
        p <- p + geom_errorbarh(data = qp$thresholds, height = .03,
                                aes(xmin = .data$threinf, xmax = .data$thresup, color = .data[[color]],  y = .data$prob))
      }
    }
    else {
      if (averages) p <- p + geom_point(data = qp$averages, aes(x = .data[[qp$x_str]], y = .data$prob))
      if (curves) p <- p + geom_line(data = qp$curves, aes(x = .data$x, y = .data$y))
      if (thresholds) p <- p + geom_linerange(data = qp$thresholds, aes(x = .data$thre, ymin = qp$guess, ymax = .data$prob))
      if (ci) p <- p + geom_errorbarh(data = qp$thresholds, height = .03, aes(xmin = .data$threinf, xmax = .data$thresup, y = .data$prob))
    }
  }

  p
}
