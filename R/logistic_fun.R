#' @export
logistic_fun <- function(x,p) (1 + exp(-p[2] * (x - p[1])))^(-1)

