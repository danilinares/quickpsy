#' Fits psychometric functions
#'
#' \code{quickpsy} fits psychometric functions using maximum likelihood.
#' @param d Data frame with the data to fit. It should have a
#' \href{http://vita.had.co.nz/papers/tidy-data.html}{tidy} form in which
#' each column corresponds to a variable and each row is an observation.
#' @param x Name of the explanatory variable.
#' @param k Name of the response variable. The response variable could be the
#' number of trials in which a response was given or a vector of 0s (or -1s) and
#' 1s indicating the response on each trial.
#' @param n Only necessary if \code{k} refers to the number of trials of a given
#' response. It corresponds to the name of the variable indicating the total
#' number of trials.
#' @param grouping Name of the grouping variables. It should be specified as
#' \code{grouping = .(variable_name1, variable_name2)}.
#' @param random Name of the random variable. It should be specified as
#' \code{random = .(variable_name1, variable_name2)}. In the current version
#' of quickpsy, the random variable has not special treatment. It does the
#' same as \code{grouping}.
#' @param within Name of the within variable. It should be specified as
#' \code{within = .(variable_name1, variable_name2)}. In the current version
#' of quickpsy, the within variable has not special treatment. It does the
#' same as \code{grouping}.
#' @param between Name of the between variable.  It should be specified as
#' \code{between = .(variable_name1, variable_name2)}. In the current version
#' of quickpsy, the between variable has not special treatment. It does the
#' same as \code{grouping}.
#' @param xmin Minimum value of the explanatory variable for which the curves
#' should be calculated (the default is the minimum value of the explanatory
#' variable).
#' @param xmax Maximum value of the explanatory variable for which the curves
#' should be calculated (the default is the maximum value of the explanatory
#' variable).
#' @param log If \code{TRUE}, the logarithm of the independent variable is used
#' to fit the curves (default is \code{FALSE}).
#' @param fun Name of the shape of the curve to fit. It could be a predefined
#' shape (\code{cum_normal_fun}, \code{logistic_fun}, \code{weibull_fun})
#' or the name of a function introduced by the user
#' (default is \code{cum_normal_fun}).
#' @param pini Initial parameters. quickpsy calculates default
#' initial parameters using probit analysis, but it is also possible to
#' specify a vector of initial parameters or a list of the form
#' \code{list(c(para1min, para2max), c(para2min, para2max))} to
#' constraint the lower and upper bounds of the parameters (when
#' \code{optimization = 'DE'}, pini should be also a list).
#' @param guess Guess rate (default is 0).
#' @param lapses Lapse rate (default is 0).
#' @param prob Probability to calculate the threshold (default is
#' \code{guess + .5 * (1 - guess)}).
#' @param thresholds If \code{FALSE}, thresholds are not calculated
#' (default is \code{TRUE}).
#' @param logliks If \code{TRUE}, the loglikelihoods are calculated
#'  (default is \code{FALSE}).
#' @param bootstrap \code{'parametric'} performs parametric bootsrap;
#' \code{'nonparametric'} performs non-parametric bootstrap;
#' \code{'none'} does not perform bootstrap (default is \code{'parametric'}).
#' @param B number of bootstrap samples (default is ONLY a 100).
#' @param ci Confidence intervals level based on percentiles (default is .95).
#' @param optimization Method to optimize. The default is 'optim' which uses
#' the \code{optim} function. It can also be \code{'DE'} which uses de function
#' \code{DEoptim} from the package DEoptim, which performs differential
#' evolution optimization. Using \code{DEoptim}, it is less likely that the
#' optimization finishes in a local minimum, but the optimization is slow.
#' When \code{'DE'} is used, \code{pini} should be specified as a list with
#' lower and upper bounds.
#' @return A list containing the following components:
#' \itemize{
#'   \item \code{x, k, n}
#'   \item \code{groups} The grouping variables.
#'   \item \code{funname} String with the name of the shape of the curve.
#'   \item \code{psyfunguesslapses} Curve including guess and lapses.
#'   \item \code{limits} Limits of the curves.
#'   \item \code{pini} Initial parameters.
#'   \item \code{optimization} Method to optimize.
#'   \item \code{piniset} \code{FALSE} if initial parameters are not given.
#'   \item \code{ypred} Predicted probabilities at the values of the explanatory
#'   variable.
#'   \item \code{curves} Curves.
#'   \item \code{para} Fitted parameters
#'   \item \code{paraci} Confidence intervals for the parameters
#'   \item \code{thresholds} Thresholds.
#'   \item \code{thresholdsci} Confidence intervals for the thresholds.
#' }
#' @seealso \code{\link{quickpsy_}}
#' @examples
#' library(MPDiR) # contains the Vernier data
#' data(Vernier) # ?Venier for the reference
#' fit <- quickpsy(Vernier, Phaseshift, NumUpward, N,
#'                 grouping = .(Direction, WaveForm, TempFreq))
#' plotcurves(fit)
#' plotpara(fit)
#' plotthresholds(fit)
#' @export
#'
quickpsy <- function(d, x = x, k = k, n = n, grouping, random, within, between,
                     xmin = NULL, xmax = NULL, log = F,
                     fun = cum_normal_fun,pini = NULL, guess = 0, lapses = 0,
                     prob = NULL, thresholds = T, logliks = F,
                     bootstrap = 'parametric', B = 100, ci = .95,
                     optimization = 'optim') {

  x <- deparse(substitute(x))
  k <- deparse(substitute(k))
  fun <- deparse(substitute(fun))
  if (!missing(n)) n <- deparse(substitute(n))
  else n <- NULL
  if (!missing(random)) random <- as.character(substitute(random))[-1]
  if (!missing(within)) within <- as.character(substitute(within))[-1]
  if (!missing(between)) between <- as.character(substitute(between))[-1]
  if (!missing(grouping)) grouping <- as.character(substitute(grouping))[-1]

  ### calling the standard evaluation of quickpsy
  quickpsy_(d, x, k, n, grouping, random, within, between, xmin, xmax, log, fun,
            pini, guess, lapses, prob, thresholds, logliks, bootstrap,
            B, ci, optimization)
}
