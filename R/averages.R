#' Perform the averages
#' \code{averages} perform the averages
#' @keywords internal
#' @param  d The input data frame.
#' @param x Name of the explanatory variable.
#' @param x_str String with the name of the explanatory variable.
#' @param k  Name of the response variable.
#' @param n Only necessary if \code{k} refers to the number of trials
#' in which a yes-type response was given. It corresponds to the name of the
#' variable indicating the total number of trials.
#' @param log If \code{TRUE}, the logarithm in base 10 of the explanatory
#' variable is used to fit the curves (default is \code{FALSE}).
#' @param grouping Name of the grouping variables. It should be specified as
#' \code{grouping = c("variable_name1", "variable_name2")}.
#' @importFrom rlang :=
averages <- function(d, x, x_str, k, n, log, grouping) {

  grouping_x <- c(grouping, x_str)

  averages <- d %>%
    group_by(!!!syms(grouping_x))


  if (is.null(n)) {
    averages <- averages %>%
      summarise(n = n(), k = sum(!!k), .groups = "drop_last")
  }
  else {
    averages <- averages %>%
      summarise(n = sum(!!n), k = sum(!!k), .groups = "drop_last")
  }

  averages <- averages %>% mutate(prob = k / n)

  if (log) {
    averages <- averages %>%
      mutate(!!quo_name(x) := log(!!x))
    }

  averages
}


