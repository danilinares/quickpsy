#' inv_weibull_fun
#'
#' @export
inv_weibull_fun <- function(x, p) qweibull(x, p[2], p[1])
