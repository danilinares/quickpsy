#' Calculates the loglikelihoods
#' \code{logliks} calculates the loglikelihoods.
#' @param nll_fun The \code{nll_fun} data frame from quickpsy.
#' @param param The \code{param} data frame from quickpsy.
#' @param grouping_without_fun The \code{grouping_without_fun} data frame from quickpsy.
logliks <- function(nll_fun, param, grouping_without_fun) {
  one_loglik <- function(nll_fun, param) {

    nll <- nll_fun$nll_fun[[1]](param$par)

    data.frame(loglik = -nll, n_par = length(param$par))
  }

  nll_fun_n <- nll_fun %>% nest_by(.key = "nll_fun")
  param_n <- param %>% nest_by(.key = "param")

  nll_fun_n %>%
    left_join(param_n, by = grouping_without_fun) %>%
    summarise(one_loglik(nll_fun, param), .groups= "keep")

}


