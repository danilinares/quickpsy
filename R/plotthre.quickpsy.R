#' plotthre.quickpsy
#'
#' @export
plotthre.quickpsy <- function(qp, xpanel, ypanel, color) {
  if (!('thresholds' %in% names(qp))) stop('quickpsy should be called with thre = TRUE')

  p <- ggplot()

  groups <- qp$groups
  ngroup <- length(groups)

  if (ngroup == 1) {
    fill <- groups[[1]]
    p <- p +
      geom_bar(data = qp$thresholds,
               aes_string(x =fill, fill = fill, y = 'thre'),
               stat = 'identity', position = 'dodge')
  }
  if (ngroup == 2) {
    fill <- groups[[1]]
    x <- groups[[2]]
    qp$thresholds[[fill]] <- factor(qp$thresholds[[fill]])
    p <- p +
      geom_bar(data = qp$thresholds,
               aes_string(x =x, fill = fill, y = 'thre'),
               stat = 'identity', position = 'dodge')
  }
  if (ngroup == 3) {
    fill <- groups[[1]]
    x <- groups[[2]]
    xpanel <- groups[[3]]
    qp$thresholds[[fill]] <- factor(qp$thresholds[[fill]])
    p <- p +
      facet_wrap(as.formula(paste0('~',xpanel))) +
      geom_bar(data = 1p$thresholds,
               aes_string(x =x, fill = fill, y = 'thre'),
               stat = 'identity', position = 'dodge')
  }
  p
}
