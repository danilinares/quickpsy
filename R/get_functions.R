#' Predefined functions
#'
#' \code{getfunctions} lists the predefined functions in \code{quickpsy}.
#'
#' @export
get_functions <- function() {
  list(cum_normal_fun = cum_normal_fun,
       logistic_fun = logistic_fun,
       weibull_fun = weibull_fun)
}
