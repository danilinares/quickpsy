#' Fits psychometric functions that share parameters
#'
#' \code{quickpsys} fits psychometric functions that share parameters
#' @param d Data frame with the results of a Yes-No experiment to fit.
#' It should have a
#' \href{http://vita.had.co.nz/papers/tidy-data.html}{tidy} form in which
#' each column corresponds to a variable and each row is an observation.
#' @param x Name of the explanatory variable.
#' @param k Name of the response variable. The response variable could be the
#' number of trials in which a yes-type response was given or a vector of 0s
#' (or -1s; no-type response) and 1s (yes-type response) indicating the
#' response on each trial.
#' @param n Only necessary if \code{k} refers to the number of trials
#' in which a yes-type response was given. It corresponds to the name of the
#' variable indicating the total number of trials.
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
#' @param log If \code{TRUE}, the logarithm of the explanatory variable is used
#' to fit the curves (default is \code{FALSE}).
#' @param fun Vector of string names of the functions introduced by the user 
#' @param parini Initial parameters. It could be a vector or a list of the form
#' \code{list(c(par1min, par2max), c(par2min, par2max))} to
#' constraint the lower and upper bounds of the parameters (when
#' \code{optimization = 'DE'}, parini should be also a list).
#' @param prob Probability to calculate the threshold (default is
#' \code{guess + .5 * (1 - guess)}).
#' @param thresholds If \code{FALSE}, thresholds are not calculated
#' (default is \code{TRUE}).
#' @param logliks If \code{TRUE}, the loglikelihoods are calculated
#'  (default is \code{FALSE}).
#' @param bootstrap \code{'parametric'} performs parametric bootstrap;
#' \code{'nonparametric'} performs non-parametric bootstrap;
#' \code{'none'} does not perform bootstrap (default is \code{'parametric'}).
#' @param B number of bootstrap samples (default is 100 ONLY).
#' @param ci Confidence intervals level based on percentiles (default is .95).
#' @param optimization Method used for optimizization. The default is 'optim' which uses
#' the \code{optim} function. It can also be \code{'DE'} which uses de function
#' \code{DEoptim} from the package DEoptim, which performs differential
#' evolution optimization. By using \code{DEoptim}, it is less likely that the
#' optimization finishes in a local minimum, but the optimization is slow.
#' When \code{'DE'} is used, \code{parini} should be specified as a list with
#' lower and upper bounds.
#' @return A list containing the following components:
#' \itemize{
#'   \item \code{x, k, n}
#'   \item \code{groups} The grouping variables.
#'   \item \code{funname} String with the name of the shape of the curve.
#'   \item \code{limits} Limits of the curves.
#'   \item \code{parini} Initial parameters.
#'   \item \code{optimization} Method to optimize.
#'   \item \code{pariniset} \code{FALSE} if initial parameters are not given.
#'   \item \code{ypred} Predicted probabilities at the values of the explanatory
#'   variable.
#'   \item \code{curves} Curves.
#'   \item \code{para} Fitted parameters.
#'   \item \code{paraci} Confidence intervals for the parameters.
#'   \item \code{curvesbootstrap} Bootstrap curves.
#'   \item \code{thresholds} Thresholds.
#'   \item \code{thresholdsci} Confidence intervals for the thresholds.
#'   \item \code{logliks} Log-likelihoods of the model.
#'   \item \code{loglikssaturated} Log-likelihoods of the saturated model.
#'   \item \code{deviance} Deviance of the model and the p-value calculated by
#'    bootstraping.
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
#' @seealso \code{\link{quickpsys_}}
#' @examples
#' # make sure that all the requires packages are installed
#' # and loaded; instructions at https://github.com/danilinares/quickpsy
#' library(MPDiR) # contains the Vernier data; use ?Vernier for the reference
#' fit <- quickpsy(Vernier, Phaseshift, NumUpward, N,
#'                 grouping = .(Direction, WaveForm, TempFreq), B = 10)
#' plotcurves(fit)
#' plotpar(fit)
#' plotthresholds(fit, geom = 'point')
#' @export
#' @import MPDiR
#' @importFrom  graphics par
#' @importFrom stats approx as.formula lm median optim pnorm pweibull qnorm
#' quantile qweibull rbinom
#' @importFrom utils combn head read.table tail


quickpsys <- function(d, x = x, k = k, n = n, grouping, random, within, between,
                     xmin = NULL, xmax = NULL, log = FALSE,
                     fun = NULL, parini = NULL,
                     prob = NULL, thresholds = T,
                     bootstrap = 'parametric', B = 100, ci = .95,
                     optimization = 'optim') {

  x <- deparse(substitute(x))
  k <- deparse(substitute(k))
  if (!missing(n)) n <- deparse(substitute(n))
  else n <- NULL
  if (!missing(random)) random <- as.character(substitute(random))[-1]
  if (!missing(within)) within <- as.character(substitute(within))[-1]
  if (!missing(between)) between <- as.character(substitute(between))[-1]
  if (!missing(grouping)) grouping <- as.character(substitute(grouping))[-1]


  ### calling the standard evaluation of quickpsy
  quickpsys_(d, x, k, n, grouping, random, within, between, xmin, xmax, log, fun,
            parini, prob, thresholds, bootstrap,
            B, ci, optimization)
}

#' Data set for demonstration
#'
#' It is part of the data associated with the paper 'Motion signal and
#' the perceived positions of moving objects'.
#' @name qpdat
#' @docType data
#' @references Linares, D., López-Moliner, J., & Johnston, A. (2007). Motion
#'signal and the perceived positions of moving objects. Journal of Vision,
#' 7(7), 1.

'qpdat'


