#' Creates bootstrap samples
#'
#' \code{avbootstrap} creates bootstrap samples
#' @param averages The \code{averages} data frame from quickpsy.
#' @param ypred The \code{ypred} data frame from quickpsy.
#' @param bootstrap \code{"parametric"} performs parametric bootstrap;
#' \code{"nonparametric"} performs non-parametric bootstrap;
#' \code{"none"} does not perform bootstrap (default is \code{'parametric'}).
#' @param B number of bootstrap samples (default is 100 ONLY).
#' @importFrom stats rbinom
avbootstrap <- function(averages, ypred, bootstrap, B) {

  if (bootstrap == "parametric") averages$y <- ypred$y
  if (bootstrap != "parametric") averages$y <- averages$prob

  averages$prob <- NULL

  data.frame(sample = 1:B) %>%
    crossing(averages) %>%
    group_by(sample) %>%
    mutate(k = rbinom(n(), size = n, prob = .data$y), prob = .data$k / n) %>%
    dplyr::select(-.data$y) %>%
    group_by(sample)
}


