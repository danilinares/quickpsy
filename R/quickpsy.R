#' Fits psychometric functions.
#'
#' \code{quickpsy} fits psychometric functions.
#' @param d Data frame with the data to fit.
#' @param x Name of the independent variable.
#' @param k Name of the response variable. The response variable could be the
#' number of trials of a given response or a vector of 0s and 1s indicating
#' the response on each trial.
#' @param n Only necessary if k refers to the number of trials of a given
#' response. It corresponds to the name of the variable indicatingthe total
#' number of trials.
#' @param random Name of the random variable. It should be introduced as
#' .(variable_name).
#' @param within Name of the within variable. It should be introduced as
#' .(variable_name).
#' @param between Name of the between variable. It should be introduced as
#' .(variable_name).
#' @param xmin Minimum value of the independent variable for which the curve
#' should be calculated (default is minimum value of the independent variable).
#' @param xmax Maximum value of the independent variable for which the curve
#' should be calculated (default is maximum value of the independent variable).
#' @param fun Name of the shape of the curve to fit. It could be a predefined
#' shape (cum_normal_fun, logistic_fun, weibull_fun) or a function introduced
#' by the user.
#' @param pini Initial values of the parameters.
#' @param guess Guess rate (default is 0).
#' @param lapses Lapse rate (default is 0).
#' @param prob Probability to calculate the threshold (default is
#' \code{guess + .5 * (1 - guess)}).
#' @param thresholds If \code{FALSE}, thresholds are not calculated.
#' @param logliks If \code{FALSE}, logliks are not calculated.
#' @param bootstrap \code{'parametric'} parametric bootsrap;
#' \code{'nonparametric'} non-parametric bootstrap; \code{'none'} No bootstrap.
#' @return A list containing the following components:
#' \itemize{
#'   \item \code{x, k, n}
#'   \item \code{groups} The grouping variables.
#'   \item \code{funname} String with the name of the shape of the curve
#'   \item \code{psyfunguesslapses} Psychometric function
#'   \item \code{limits} Limits of the curves.
#'   \item \code{pini} Initial parameters.
#'   \item \code{DE} Use of Differential Evolution algorithm (DEoptim).
#'   \item \code{piniset} \code{FALSE} if initial parameters are not given.
#'   \item \code{ypred} Predicted probabilities at the \code{x} values.
#'   \item \code{curves} Psychometric curves.
#'   \item \code{para} Fitted parameters
#'   \item \code{paraci} Confidence intervals for the parameters
#'   \item \code{thresholds} Thresholds.
#'   \item \code{thresholdsci} Confidence intervals for the thresholds.
#' }
#' @seealso \code{\link{quickpsy_}}
#' @examples
#'
#' data(quickpsydat)
#' fit <- quickpsy(quickpsydat, FASE, RESP,
#'                 between = .(ECC, INTERVAL, obs), B = 100)
#' plotcurves(fit)
#' @export

#'
quickpsy <- function(d = d, x = x, k = k, n = n, random, within, between,
                     xmin = NULL, xmax = NULL, log = F, fun = cum_normal_fun,
                     pini = NULL, guess = 0, lapses = 0, prob = NULL,
                     thresholds = T, logliks = F,
                     bootstrap = 'parametric', B = 1000, ci = .95, DE = F) {

  x <- deparse(substitute(x))
  k <- deparse(substitute(k))
  fun <- deparse(substitute(fun))
  if (!missing(n)) n <- deparse(substitute(n))
  else n <- NULL
  if (!missing(random)) random <- as.character(substitute(random))[-1]
  if (!missing(within)) within <- as.character(substitute(within))[-1]
  if (!missing(between)) between <- as.character(substitute(between))[-1]

  ### calling the standard evaluation of quickpsy
  quickpsy_(d, x, k, n, random, within, between, xmin, xmax, log, fun, pini,
            guess, lapses, prob, thresholds, logliks, bootstrap,
            B, ci, DE)
}


