#' plotthre.quickpsy
#'
#' @export
plotthre.quickpsy <- function(l, xpanel, ypanel, color) {
  if (!('thresholds' %in% names(fit))) stop('quickpsy should be called with thre = TRUE')

  p <- ggplot()

  ngroup <- length(l$grouping_var)

  if (ngroup == 0) {
    p <- p +
      geom_bar(data = l$thresholds,
               aes_string(x = 0,y = 'thre'),
               stat = 'identity', position = 'dodge')
  }
  if (ngroup == 1) {
    fill <- l$grouping_var[1]
    p <- p +
      geom_bar(data = l$thresholds,
               aes_string(x =fill, fill = fill, y = 'thre'),
               stat = 'identity', position = 'dodge')
  }
  if (ngroup == 2) {
    fill <- l$grouping_var[1]
    x <- l$grouping_var[2]
    l$thresholds[[fill]] <- factor(l$thresholds[[fill]])
    p <- p +
      geom_bar(data = l$thresholds,
               aes_string(x =x, fill = fill, y = 'thre'),
               stat = 'identity', position = 'dodge')
  }
  if (ngroup == 3) {
    fill <- l$grouping_var[1]
    x <- l$grouping_var[2]
    xpanel <- l$grouping_var[3]
    l$thresholds[[fill]] <- factor(l$thresholds[[fill]])
    p <- p +
      facet_wrap(as.formula(paste0('~',xpanel))) +
      geom_bar(data = l$thresholds,
               aes_string(x =x, fill = fill, y = 'thre'),
               stat = 'identity', position = 'dodge')
  }
  p
}
