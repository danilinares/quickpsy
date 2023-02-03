#' @keywords internal

quickpsy_without_bootstrap <- function(averages, x, x_str, k, n,
                     grouping,
                     xmin, xmax,
                     log,
                     fun,
                     funname,
                     parini,
                     guess, lapses,
                     prob, thresholds,
                     control,
                     parinivector,
                     paircomparisons,
                     line_res,
                     method,
                     binomial_coef) {


  ### Groups


  if (is.function(fun)) grouping_fun <- character(0) #NULL #c()
  else if (is.data.frame(fun)) {
    grouping_fun <- fun %>% select(-fun) %>% names()
  }
  else stop("Argument fun should be a function or a data frame of functions")

  grouping_without_fun <- grouping %>% setdiff(grouping_fun)

  ### Calling functions

  averages <- averages %>% group_by(!!!syms(grouping))

  limits <- limits(averages, x_str, xmin, xmax)

  psych_fun <- psych_fun(fun, guess, lapses, grouping_fun)

  nll_fun <- nll_fun(averages, psych_fun, x_str, binomial_coef, grouping_without_fun, grouping_fun)

  nll_fun_saturated <- nll_fun_saturated(averages, psych_fun, binomial_coef, grouping_without_fun)


  if (is.null(parini) & funname %in% names(get_functions()) ) {
    parini <- calculate_parini(averages, funname, x, guess, lapses, grouping)
   }
  else if (is.null(parini) & !(funname %in% names(get_functions())) ){
    stop("parini (initial parameters) must be specified.", call. = FALSE)
  }
  else {
    parini <- parini(averages, parini, psych_fun, grouping_without_fun)
  }

  param_hessian <- param(nll_fun, parini, control, parinivector, grouping_without_fun, funname, guess, lapses, method)

  param <- param_hessian |> select(-hessian) |> unnest(param)

  hessian <- param_hessian |> select(-param)

  ypred <- ypred(averages, param, psych_fun, x_str, log,
                 grouping, grouping_without_fun, grouping_fun,
                 funname, guess, lapses)

  x_seq <- x_seq(limits, x, grouping, line_res)

  curves <- ypred(x_seq, param, psych_fun, x_str, log,
                  grouping, grouping_without_fun, grouping_fun,
                  funname, guess, lapses)

  logliks <- logliks(nll_fun, param, grouping_without_fun)


  loglikssaturated <- loglikssaturated(nll_fun_saturated, averages, grouping_without_fun)

  aic <- akaike(logliks)

  deviance <- devi(logliks, loglikssaturated, grouping_without_fun)


  qp <- list(averages = averages,
             limits = limits,
             psych_fun = psych_fun,
             nll_fun = nll_fun,
             nll_fun_saturated = nll_fun_saturated,
             parini = parini,
             par = param,
             hessian = hessian,
             ypred = ypred,
             x_seq = x_seq,
             curves = curves,
             logliks = logliks,
             loglikssaturated = loglikssaturated,
             aic = aic,
             deviance = deviance)

  if (thresholds) {
    thre <- thresholds(param, curves, funname, psych_fun, prob, log, guess, lapses, grouping)
    qp$thresholds <- thre
  }

  if (paircomparisons) {

    if (length(grouping) > 0) {
      param_dif <- param_dif(param)
      qp <- c(qp, list(par_dif = param_dif))
    }

    if (thresholds & (length(grouping) > 0)) {
      thresholds_dif <- thresholds_dif(thre)
      qp <- c(qp, list(thresholds_dif = thresholds_dif))
    }
  }

  qp

}

