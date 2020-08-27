#' \code{nll_fun} Creates the negative log-likelihood function
#' @keywords internal
#' @param averages The \code{averages} data frame from quickpsy.
#' @param psych_fun The \code{psych_fun} data frame from quickpsy.
#' @param x_str The \code{x_str} data frame from quickpsy.
#' @param grouping_without_fun The \code{grouping_without_fun} data frame from quickpsy.
#' @param grouping_fun The \code{grouping_fun} data frame from quickpsy.
nll_fun <- function(averages, psych_fun, x_str, grouping_without_fun, grouping_fun) {

  averages %>%
    group_by(!!!syms(grouping_without_fun)) %>%
    nest(averages = !group_cols()) %>%
    mutate(psych_fun = list(psych_fun)) %>%
    rowwise() %>%
    mutate(nll_fun = list(create_nll(averages, psych_fun, x_str, grouping_fun))) %>%
    select(-c(averages, psych_fun)) %>%
    group_by(!!!syms(grouping_without_fun))

}
