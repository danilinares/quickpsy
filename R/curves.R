#' curves
#'
#' @export
curves <- function(qp, xmin = NULL, xmax = NULL, log = F) {
  one_curve <- function(d, fun, log) {
    xseq <- seq(unique(d$xmin), unique(d$xmax), length = 1000)
    yseq <- fun(xseq, d$para)
    if (log) xseq <- exp(xseq)
    data.frame(x = xseq, y = yseq)
  }

  ngroup <- length(qp$groups)

  if (ngroup == 0) paramlimits <- cbind(qp$param, qp$limits)
  else paramlimits <- inner_join(qp$param, qp$limits)

  if (!is.null(xmin)) paramlimits$xmin <- xmin
  if (!is.null(xmax)) paramlimits$xmax <- xmax

  paramlimits %>% do(one_curve(., qp$fun, log))

}
