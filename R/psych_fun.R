#' \code{psy_fun} psych_fun
#' @keywords internal
psych_fun <- function(fun, guess, lapses, grouping_fun) {

  psy_fun <- create_psy_fun(fun, guess, lapses)

  if (is.function(fun)) tibble(fun = list(psy_fun))
  else fun %>% group_by(!!!syms(grouping_fun))

}
