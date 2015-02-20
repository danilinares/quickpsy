#' fit_main_glm
#'
#' @inheritParams create_nll
#' @param pini Initial parameters.
#' @export
fit_main_glm <- function(d, x, k, n, psy_fun, psy_fun_name, pini, guess, lapses) {
  m <- round(1 / guess)
  print(m)
  model <- glm(cbind(d[[k]], d[[n]] - d[[k]]) ~ d[[x]],
               family = binomial(mafc.probit(2)))

  c(-coef(model)[[1]] / coef(model)[[2]], 1/coef(model)[[2]])
}

