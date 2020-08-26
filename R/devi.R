#' Calculates the deviance
#'
#' \code{devi} calculates the deviance
devi <- function(logliks, loglikssaturated, grouping_without_fun) {

  one_devi <- function(logliks, loglikssaturated) {
    deviance <-  2 * (loglikssaturated$loglik - logliks$loglik)
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
