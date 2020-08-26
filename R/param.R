#' \code{param} Param
#' @keywords internal
param <- function(nll_fun, parini, control, parinivector, grouping_without_fun) {

  calculate_par <- function(parini, nll_fun, control, parinivector) {

    if ("par" %in% names(parini)) {

      param <- optim(parini$par, nll_fun, control = control)$par
    }
    else {
      if (is.null(parinivector)) p <- .5 * (parini$parmax - parini$parmin)
      else p <- parinivector

      param <- optim(p, nll_fun, method = "L-BFGS-B",
                     lower = parini$parmin,
                     upper = parini$parmax,
                     control = control)$par
    }
    data.frame(parn = paste0("p", seq(1, length(param))), par = param)
  }

  parini %>%
    group_by(!!!syms(grouping_without_fun)) %>%
    nest(parini = !group_cols()) %>%
    left_join(nll_fun, by = grouping_without_fun) %>%
    rowwise() %>%
    summarise(calculate_par(parini, nll_fun, control, parinivector), .groups = "keep")

}
