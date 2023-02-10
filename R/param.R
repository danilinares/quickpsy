#' \code{param} Param
#' @keywords internal
#' @param nll_fun The \code{nll_fun} data frame from quickpsy.
#' @param parini The \code{parini} data frame from quickpsy.
#' @param control The \code{control} data frame from quickpsy.
#' @param parinivector The \code{parinivector} data frame from quickpsy.
#' @param grouping_without_fun The \code{grouping_without_fun} data frame from quickpsy.
#' @param funname The \code{funname} data frame from quickpsy.
#' @param guess Argument \code{guess}
#' @param lapses Argument \code{lapses}
#' @importFrom stats optim

param <- function(nll_fun, parini, control, parinivector, grouping_without_fun, funname,
                  guess, lapses, method) {


  calculate_par <- function(parini, nll_fun, control, parinivector, method) {


    if ("par" %in% names(parini)) {

      if (method == "optim") {
        fit <- optim(parini$par, nll_fun, control = control,
                       hessian = TRUE)
      }
      # if (method == "nlopt") { # we need to revover the hessian and the solution
      #   param <- nloptr(parini$par, nll_fun,
      #                   opts = list("algorithm"="NLOPT_LN_SBPLX",
      #                               "xtol_rel"=1.0e-10)
      #                   )$solution
      # }
    }
    else {
      if (is.null(parinivector)) p <- .5 * (parini$parmax - parini$parmin)
      else p <- parinivector

      fit <- optim(p, nll_fun, method = "L-BFGS-B",
                     lower = parini$parmin,
                     upper = parini$parmax,
                     control = control)
    }

    param <- fit$par

    if (funname %in% names(get_functions())) {
      if (is.logical(guess) && is.logical(lapses)) {
        param[3] <- exp(param[3])
        param[4] <- exp(param[4])
      }
      if (is.logical(guess) && is.numeric(lapses)) {
        param[3] <- exp(param[3])
      }
      if (is.logical(lapses) && is.numeric(guess)) {
        param[3] <- exp(param[3])
      }
    }

    param <- tibble(parn = paste0("p", seq(1, length(param))),
                        par = param)

    tibble(param = list(param), hessian = list(fit$hessian))
  }

  parini %>%
    group_by(!!!syms(grouping_without_fun)) %>%
    nest(parini = !group_cols()) %>%
    left_join(nll_fun, by = grouping_without_fun) %>%
    rowwise() %>%
    summarise(calculate_par(parini, nll_fun, control, parinivector, method), .groups = "keep")

}
