#' \code{nll_fun} Creates the sturated negative log-likelihood function
#' @keywords internal
#' @param averages The \code{averages} data frame from quickpsy.
#' @param psych_fun The \code{psych_fun} data frame from quickpsy.
#' @param grouping_without_fun The \code{grouping_without_fun} data frame from quickpsy.
nll_fun_saturated <- function(averages, psych_fun, binomial_coef, grouping_without_fun) {

  averages %>%
    group_by(!!!syms(grouping_without_fun)) %>%
    nest(averages = !group_cols()) %>%
    rowwise() %>%
    mutate(nll_fun = list(create_nll_saturated(averages, binomial_coef))) %>%
    select(-averages) %>%
    group_by(!!!syms(grouping_without_fun))

}
