#' parameters
#'
#' @export
parameters <- function(d, x, k, n, psyfunguesslapses, funname,
                    pini, piniset, guess, lapses, pini2, groups) {
  d %>% do(one_parameters(., x, k, n, psyfunguesslapses, funname,
                  pini, piniset, guess, lapses, pini2, groups))
}
