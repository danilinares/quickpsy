#' It creates from the data the negative log-likelihood as a function of the parameters.
#' @param d The data frame containing the data.
#' @param x Name of the independent variable.
#' @param k Number of 'yes' or correct responses.
#' @param n Number of trials.
#' @param psy_fun Function to create the negative log-likelihood.
#' @return It returns a function. The negative log-likelihood as a function of the parameters (p).
#' @export
create_nll <- function(d, x, k, n, psy_fun){
  function(p) {
    phi <- psy_fun(d[[x]], p)
    if (any(phi <= 0) || any(phi >=1) || any(is.na(phi))) return(-Inf)
    else return(-sum(d[[k]] * log(phi) + (d[[n]] - d[[k]]) * log(1 - phi)))
  }
}
