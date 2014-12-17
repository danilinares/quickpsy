#' inv_logistic_fun
#'
#' @export
inv_logistic_fun <- function(q, p) p[1] + 1 / p[2] * log(q / (1 - q))
