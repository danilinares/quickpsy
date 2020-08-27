#' Fits psychometric functions
#'
#' \code{quickpsy} fits, by direct maximization of the likelihood
#' (Prins and Kingdom, 2010; Knoblauch and Maloney, 2012),
#'  psychometric functions of the form
#' \deqn{\psi(x) = \gamma + (1 - \gamma - \lambda) * fun(x)}
#' where \eqn{\gamma} is the guess rate, \eqn{\lambda} is the lapse rate and
#' \eqn{fun} is a sigmoidal-shape function with asymppotes at 0 and 1.
#'
#' @param d Data frame with a
#' \href{http://vita.had.co.nz/papers/tidy-data.html}{tidy} form in which
#' each column corresponds to a variable and each row is an observation.
#' @param x Name of the explanatory variable.
#' @param k Name of the response variable. It could be the
#' number of trials in which a yes-type response was given or a vector of 0s
#' (no-type response) and 1s (yes-type response) indicating the
#' response on each trial.
#' @param n Only necessary if \code{k} refers to the number of trials
#' in which a yes-type response was given. It corresponds to the name of the
#' variable indicating the total number of trials.
#' @param grouping Name of the grouping variables. It should be specified as
#' \code{grouping = c("variable_name1", "variable_name2")}.
#' @param xmin Minimum value of the explanatory variable for which the curves
#' should be calculated (the default is the minimum value of the explanatory
#' variable).
#' @param xmax Maximum value of the explanatory variable for which the curves
#' should be calculated (the default is the maximum value of the explanatory
#' variable).
#' @param log If \code{TRUE}, the logarithm in base 10 of the explanatory
#' variable is used to fit the curves (default is \code{FALSE}).
#' @param fun Name of the shape of the curve to fit. It could be a predefined
#' shape (\code{cum_normal_fun}, \code{logistic_fun}, \code{weibull_fun})
#' or the name of a function introduced by the user
#' (default is \code{cum_normal_fun}).
#' @param parini Initial parameters. quickpsy calculates default
#' initial parameters for the predefined functions by linear modelling of
#' the probit-transformed data. Otherwise, \code{parini} could be
#' \itemize{
#'   \item  a vector of initial parameters
#'   \item a list of the form
#'   \code{list(c(par1min, par1max), c(par2min, par2max))} to
#'   constraint the lower and upper bounds of the parameters
#'   \item a dataframe specifiying the initial parameters for each condition
#'   with the same structure that the output \code{par},
#'   but without the confidence intervals.
#'   \item a dataframe specifiying the lower (the column should be called
#'   parmin) and upper bound (the column should be called parmax) for
#'   each condition.
#'  }
#' @param guess Value indicating the guess rate (leftward asymptote) \eqn{\gamma}
#' (default is 0). If \code{TRUE}, the guess rate is estimated as the i + 1 parameter where
#' i corresponds to the number of parameters of \code{fun}. If, for
#' example, \code{fun} is a predefined shape with parameters p1 and p2,
#' then the guess rate corresponds to parameter p3.
#' @param lapses Value indicating the lapse rate (rightward asymptote) \eqn{\lambda}
#' (default is 0). If \code{TRUE}, the lapse rate is estimated as the i + 1 parameter where
#' i corresponds to the number of parameters of \code{fun} plus one if
#' the guess rate is estimated. If, for example, \code{fun} is a
#' predefined shape with parameters p1 and p2,
#' then the lapse rate corresponds to parameter p3. If the guess rate is also
#' estimated, p3 will be the guess rate and p4 the lapse rate.
#' @param prob Probability to calculate the threshold (default is
#' \code{guess + .5 * (1 - guess)}).
#' @param thresholds If \code{FALSE}, thresholds are not calculated
#' (default is \code{TRUE}).
#' @param bootstrap \code{'parametric'} performs parametric bootstrap;
#' \code{'nonparametric'} performs non-parametric bootstrap;
#' \code{'none'} does not perform bootstrap (default is \code{'parametric'}).
#' @param B number of bootstrap samples (default is 100 ONLY).
#' @param ci Bootstrap confidence intervals level based on percentiles (default is .95).
#' @param control \code{control} argument of the \code{optim} function.
#' @param parinivector A optional vector of initials parameters when the lower and the upper
#' bounds of the parameter are specified.
#' @param paircomparisons If \code{TRUE} bootstrap paircomparions of the parameters are performed.
#' Default is \code{FALSE}
#' @return A list containing the following components:
#' \itemize{
#'   \item \code{x, k, n}
#'   \item \code{grouping} The grouping variables.
#'   \item \code{funname} String with the name of the shape of the curve.
#'   \item \code{psyfunguesslapses} Curve including guess and lapses.
#'   \item \code{limits} Limits of the curves.
#'   \item \code{parini} Initial parameters.
#'   \item \code{ypred} Predicted probabilities at the values of the explanatory
#'   variable.
#'   \item \code{curves} Psychometric curves.
#'   \item \code{par} Fitted parameters and its confidence intervals.
#'   \item \code{parcomnparisons} Pair-wise comparisons of the parameters
#'   to assess whether two parameters are significantly different
#'   using bootstrap. Specifically, the parameter bootstrap samples for each of
#'   the two conditions are substrated and then it is considered whether zero
#'   was within the confidence interval level of the distributions of differences.
#'   \item \code{curvesbootstrap} Bootstrap psychometric curves.
#'   \item \code{thresholds} Thresholds and its confidence intervals.
#'   \item \code{thresholdscomparisons} Pair-wise comparisons of the thresholds.
#'   \item \code{logliks} Log-likelihoods of the model.
#'   \item \code{loglikssaturated} Log-likelihoods of the saturated model.
#'   \item \code{deviance} Deviance of the model and the p-value calculated by
#'    using the chi-square distribution and bootstraping.
#'   \item \code{aic} AIC of the model defined as \deqn{ - 2 * loglik + 2  *k}
#'   where k is the number of parameters of the model.
#' }
#' @references
#' Burnham, K. P., & Anderson, D. R. (2003). Model selection and multimodel
#' inference: a practical information-theoretic approach. Springer Science &
#' Business Media.
#'
#' Knoblauch, K., & Maloney, L. T. (2012). Modeling Psychophysical Data in R.
#' New York: Springer.
#'
#' Prins, N., & Kingdom, F. A. A. (2016). Psychophysics: a practical
#' introduction. London: Academic Press.
#'
#' @examples
#' fit <- quickpsy(qpdat, phase, resp,
#' grouping = c("participant", "cond"), bootstrap = "none")
#' plot(fit)
#' plot(fit, color = cond)
#' plotpar(fit)
#' plotthresholds(fit, geom = "point")

#' @export
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @import ggplot2
#' @importFrom rlang .data

quickpsy <- function(d, x = x, k = k, n = NULL,
                     grouping = c(),
                     xmin = NULL, xmax = NULL,
                     log = FALSE,
                     fun = cum_normal_fun,
                     parini = NULL,
                     guess = 0, lapses = 0,
                     prob = NULL, thresholds = TRUE,
                     bootstrap = "parametric", B = 100, ci = .95,
                     control = NULL,
                     parinivector = NULL,
                     paircomparisons = FALSE) {


  ### Working with the arguments

  x <- enquo(x)

  k <- enquo(k)

  if (!missing(n)) n <- enquo(n)

  if (missing(grouping)) grouping <- character(0)

  if (is.function(fun)) funname <- deparse(substitute(fun))
  else funname <- "no_default"

  if (missing(B) & bootstrap != "none")
    cat(paste("Using only", B,
              "bootstrap samples. Use the B argument, to modify it.\n"))

  if (!is.null(prob)) thresholds <- TRUE

  if (is.logical(guess) && !guess) guess <- 0
  if (is.logical(lapses) && !lapses) lapses <- 0

  if (paircomparisons & length(grouping) == 0) {
    stop("There are no groups to perform pair comparisons. paircomparisons should be FALSE")
  }


  x_str <- quo_name(x)

  ### calculating averages

  averages <- averages(d, x, x_str, k, n, log, grouping)

  qp <- list(averages = averages)

  ### calling quickpsy one time
  qp <- c(qp, quickpsy_without_bootstrap(averages, x, x_str, k, n,
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
                             paircomparisons))


    if (bootstrap == "parametric" || bootstrap == "nonparametric") {
      avbootstrap <-  avbootstrap(qp$averages,
                                  qp$ypred, bootstrap, B)

      qp$avbootstrap <- avbootstrap

      qp_boot <- avbootstrap %>%
        nest_by() %>%
        summarise(quickpsy = list(quickpsy_without_bootstrap(.data$data, x, x_str,
                                                             quo(k), quo(n),
                                                        grouping,
                                                        xmin, xmax,
                                                        log,
                                                        fun,
                                                        funname,
                                                        qp$par,
                                                        guess, lapses,
                                                        prob, thresholds,
                                                        control, parinivector,
                                                        paircomparisons)),
                  .groups = "keep") %>%
        rowwise()



      parbootstrap <- qp_boot %>% summarise(pluck(quickpsy, "par"), .groups = "keep")

      parci <- parci(qp$par, parbootstrap, ci)

      ypredbootstrap <- qp_boot %>% summarise(pluck(quickpsy, "ypred"), .groups = "keep")

      curvesbootstrap <- qp_boot %>% summarise(pluck(quickpsy, "curves"), .groups = "keep")

      qp <- c(qp, list(avbootstrap = avbootstrap,
                       parbootstrap = parbootstrap,
                       ypredbootstrap = ypredbootstrap,
                       curvesbootstrap = curvesbootstrap))

      qp$par <- parci

      if (paircomparisons) {
        par_difbootstrap <- qp_boot %>% summarise(pluck(quickpsy, "par_dif"), .groups = "keep")

        parcomparisons <- parcomparisons(qp$par_dif, par_difbootstrap, ci)

        qp$par_difbootstrap <-  par_difbootstrap
        qp$par_dif <- parcomparisons
      }

      if (thresholds) {

        thresholdsbootstrap <- qp_boot %>% summarise(pluck(quickpsy, "thresholds"), .groups = "keep")
        thresholdsci <- thresholdsci(qp$thresholds, thresholdsbootstrap, ci)
        qp$thresholds <- thresholdsci

        qp$thresholdsbootstrap <- thresholdsbootstrap

        if (paircomparisons) {
          thresholds_difbootstrap <- qp_boot %>% summarise(pluck(quickpsy, "thresholds_dif"), .groups = "keep")

          thresholdcomparisons <- thresholdcomparisons(qp$thresholds_dif, thresholds_difbootstrap, ci)

          qp$thresholds_difbootstrap <-  thresholds_difbootstrap
          qp$thresholds_dif <- thresholdcomparisons
        }
      }


     }
#
#
# #
# #       logliksbootstrap <-  qp_boot %>%
# #         mutate(temp = map(quickpsy, "logliks")) %>%
# #         unnest(temp, .drop = TRUE)
# #
# #       loglikssaturatedbootstrap <-  qp_boot %>%
# #         mutate(temp = map(quickpsy, "loglikssaturated")) %>%
# #         unnest(temp, .drop = TRUE)
# #
# #       aicbootstrap <-  qp_boot %>%
# #         mutate(temp = map(quickpsy, "aic")) %>%
# #         unnest(temp, .drop = TRUE)
# #
# #       deviancebootstrap <-  qp_boot %>%
# #         mutate(temp = map(quickpsy, "deviance")) %>%
# #         unnest(temp, .drop = TRUE)
# #
  qp$x_str <- quo_name(x)
  qp$log <- log
  qp$guess <- guess
  qp$lapses <- lapses
  qp$grouping <- grouping



 class(qp) <- "quickpsy"
 qp
}




