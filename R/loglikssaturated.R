#' Calculates the saturated loglikelihoods
#' \code{logliks} calculates the saturatedloglikelihoods.
#' @param nll_fun_saturated The \code{nll_fun_saturated} data frame from quickpsy.
#' @param averages The \code{averages} data frame from quickpsy.
#' @param grouping_without_fun The \code{grouping_without_fun} data frame from quickpsy.
#' @export
loglikssaturated <- function(nll_fun_saturated, averages, grouping_without_fun) {
  one_loglik <- function(nll_fun_saturated, .averages) {

    nll <- nll_fun_saturated$nll_fun[[1]](.averages$prob)

    data.frame(loglik = -nll, n_par = length(.averages$k))
  }


  nll_fun_saturated_n <- nll_fun_saturated %>% nest_by(.key = "nll_fun_saturated")
  averages_n <- averages %>% group_by(!!!syms(grouping_without_fun)) %>% nest_by(.key = "averages")

  nll_fun_saturated_n %>%
    left_join(averages_n, by = grouping_without_fun) %>%
    summarise(one_loglik(nll_fun_saturated, averages), .groups = "keep")

}


