#' Calculates the deviance
#'
#' \code{devi} calculates the deviance
#' @param logliks The \code{logliks} data frame from quickpsy.
#' @param loglikssaturated The \code{loglikssaturated} data frame from quickpsy.
#' @param grouping_without_fun The \code{grouping_without_fun} data frame from quickpsy.
#' @export
#' @importFrom stats pchisq
devi <- function(logliks, loglikssaturated, grouping_without_fun) {

  one_devi <- function(logliks, loglikssaturated) {
    deviance <-  -2 * (logliks$loglik - loglikssaturated$loglik)
    df <- loglikssaturated$n_par - logliks$n_par
    p_value <-  pchisq(deviance, df, lower.tail = FALSE)
    data.frame(deviance, df, p_value)
  }

  logliks_n <- logliks %>% nest_by(.key = "logliks")
  loglikssaturated_n <- loglikssaturated %>% nest_by(.key = "loglikssaturated")

  logliks_n %>%
    left_join(loglikssaturated_n, by = grouping_without_fun) %>%
    summarise(one_devi(logliks, loglikssaturated), .groups= "keep")

}
