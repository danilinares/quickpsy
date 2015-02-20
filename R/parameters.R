#' parameters
#'
#' @export
parameters <- function(d, x, k, n, psyfunguesslapses, funname,
                    pini, piniset, guess, lapses, DE, groups) {

  d %>% dplyr::do(one_parameters(., x, k, n, psyfunguesslapses, funname,
                  pini, piniset, guess, lapses, DE, groups))
}
