#' Predicted probabilities
#'
#' \code{ypred} calculates the predicted probabilities at the values of the
#' explanatory variable.
#' @param averages The \code{"averages"} data frame from quickpsy.
#' @param param The \code{"param"} data frame from quickpsy.
#' @param psych_fun The \code{"psych_fun"} data frame from quickpsy.
#' @param x_str The \code{"x_str"} data frame from quickpsy.
#' @param log The \code{"log"} data frame from quickpsy.
#' @param grouping The \code{"grouping"} data frame from quickpsy.
#' @param grouping_without_fun The \code{"grouping_without_fun"} data frame from quickpsy.
#' @param grouping_fun The \code{"grouping_fun"} data frame from quickpsy.
ypred <- function(averages, param, psych_fun, x_str, log,
                  grouping, grouping_without_fun, grouping_fun,
                  funname, guess, lapses) {

  one_ypred <- function(averages, param, psych_fun, x_str, log) {

    if (funname %in% names(get_functions())) {
      if (is.logical(guess) && is.logical(lapses)) {
        param$par[3] <- log(param$par[3])
        param$par[4] <- log(param$par[4])
      }
      if (is.logical(guess) && is.numeric(lapses)) {
        param$par[3] <- log(param$par[3])
      }
      if (is.logical(lapses) && is.numeric(guess)) {
        param$par[3] <- log(param$par[3])
      }
    }

    x <- averages[[x_str]]
    y <- psych_fun$fun[[1]](x, param$par)
    data.frame(x, y)
  }

  averages_n <- averages %>% nest_by(.key = "averages")
  param_n <- param %>% nest_by(.key = "param")
  psych_fun_n <- psych_fun %>% nest_by(.key = "psych_fun")

  averages_n %>%
    left_join(param_n, by = grouping_without_fun) %>%
    left_join(psych_fun_n, by = grouping_fun) %>%
    group_by(!!!syms(grouping)) %>%
    rowwise() %>%
    summarise(one_ypred(averages, param, psych_fun, x_str, log), .groups = "keep")

}
