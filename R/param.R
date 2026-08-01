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


    ## Starting values and, when parini is given in list form, box bounds.
    if ("par" %in% names(parini)) {
      p  <- parini$par
      lb <- rep(-Inf, length(p))
      ub <- rep( Inf, length(p))
    }
    else {
      if (is.null(parinivector)) p <- .5 * (parini$parmax - parini$parmin)
      else p <- parinivector
      lb <- parini$parmin
      ub <- parini$parmax
    }

    if (method == "nloptr") {
      ## Derivative-free, box-constrained optimisation (COBYLA by default).
      ## Useful for stiff nonlinear models where the finite-difference gradient
      ## of L-BFGS-B goes non-finite. `control` is passed through as the nloptr
      ## `opts` list; if empty, sensible COBYLA defaults are used.
      opts <- control
      if (is.null(opts) || length(opts) == 0)
        opts <- list(algorithm = "NLOPT_LN_COBYLA", xtol_rel = 1e-6, maxeval = 1000)
      x0 <- pmin(pmax(p, lb), ub)               # keep the start inside the bounds
      nlfit <- nloptr::nloptr(x0 = x0, eval_f = nll_fun, lb = lb, ub = ub, opts = opts)
      fit <- list(par = nlfit$solution, value = nlfit$objective)
    }
    else if (any(is.finite(lb)) || any(is.finite(ub))) {
      fit <- optim(p, nll_fun, method = "L-BFGS-B",
                     lower = lb, upper = ub,
                     control = control)
    }
    else {
      fit <- optim(p, nll_fun, control = control)
    }

    # Compute the Hessian (for asymptotic CIs) separately, so that a fit which
    # converged but whose Hessian cannot be evaluated -- e.g. a parameter resting
    # on a bound, where finite differences step into a non-finite region -- keeps
    # its parameter estimates and only loses its CIs (NA), instead of aborting.
    n_par <- length(fit$par)
    hess_control <- if (method == "nloptr") list() else control  # control is nloptr opts here
    fit$hessian <- tryCatch(
      optimHess(fit$par, nll_fun, control = hess_control),
      error = function(e) {
        warning("Could not compute the Hessian for a group (", conditionMessage(e),
                "); its confidence intervals will be NA.", call. = FALSE)
        matrix(NA_real_, n_par, n_par)
      }
    )

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
