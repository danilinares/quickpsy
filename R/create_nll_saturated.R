#' Creates the saturated negative log-likelihood function
#' \code{create_nll_saturated} Creates the saturatednegative log-likelihood function
#' @keywords internal
create_nll_saturated <- function(averages) {

  averages <- averages # without this there is a data mask error

  function(p) {

    phi <- p

    phi[phi < .Machine$double.eps] <- .Machine$double.eps
    phi[phi > (1 - .Machine$double.eps)] <- 1 - .Machine$double.eps

    -sum(lchoose(averages$n, averages$k) + # includes the binomial coef
           averages$k * log(phi) + (averages$n - averages$k) * log(1 - phi))

  }
}


