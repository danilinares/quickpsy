#' It creates from the data the negative log-likelihood as a function of the parameters.
#' @param d The data frame containing the data.
#' @param x Name of the independent variable.
#' @param k Number of 'yes' or correct responses.
#' @param n Number of trials.
#' @param psy_fun Function to create the negative log-likelihood.
#' @return It returns a function. The negative log-likelihood as a function of the parameters (p).
#' @export
create_nll <- function(d, x, k, n, psyfunguesslapses){
  function(p) {
    phi <- psyfunguesslapses(d[[x]], p)
    phi[phi < .Machine$double.eps] <- .Machine$double.eps
     phi[phi > (1 - .Machine$double.eps)] <- 1 - .Machine$double.eps
       return(-sum(d[[k]] * log(phi) + (d[[n]] - d[[k]]) * log(1 - phi)))
  }
}


