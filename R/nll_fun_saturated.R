#' \code{nll_fun} Creates the sturated negative log-likelihood function
#' @keywords internal
nll_fun_saturated <- function(averages, psych_fun, grouping_without_fun) {

  averages %>%
    group_by(!!!syms(grouping_without_fun)) %>%
    nest(averages = !group_cols()) %>%
    rowwise() %>%
    mutate(nll_fun = list(create_nll_saturated(averages))) %>%
    select(-averages) %>%
    group_by(!!!syms(grouping_without_fun))

}
