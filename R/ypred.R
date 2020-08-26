#' Predicted probabilities
#'
#' \code{ypred} calculates the predicted probabilities at the values of the
#' explanatory variable.
#' @param qp output from quickpsy
#' @examples
#' library(MPDiR) # contains the Vernier data
#' data(Vernier) # ?Venier for the reference
#' fit <- quickpsy(Vernier, Phaseshift, NumUpward, N,
#'                 grouping = .(Direction, WaveForm, TempFreq), B = 20)
#' ypred(fit)
ypred <- function(averages, param, psych_fun, x_str, log, grouping, grouping_without_fun, grouping_fun) {

  one_ypred <- function(averages, param, psych_fun, x_str, log) {
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
