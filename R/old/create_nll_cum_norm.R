#' It creates from the data the negative log-likelihood as a function of the parameters.
#' @param d The data frame containing the data.
#' @param x Name of the independent variable.
#' @param k Number of 'yes' or correct responses.
#' @param n Number of trials.
#' @param psy_fun Function to create the negative log-likelihood.
#' @return It returns a function. The negative log-likelihood as a function of the parameters (p).
#' @export
create_nll_cum_norm <- function(d, x, k, n, guess, lapses){
  function(p) {
    phi <- psy_fun(d[[x]], p)
#     p1<-pnorm(d[[x]],p[[1]],p[[2]],.lower.tail=T, log.p=T)
#     p2<-pnorm(d[[x]],p[[1]],p[[2]],lower.tail=F, log.p=T)
#     print(p1)
#     print(p2)
    #return(-sum(d[[k]] * p1 + (d[[n]] - d[[k]]) * p2))
    #if (any(phi <= 0) || any(phi >=1) || any(is.na(phi))) return(-9999)
#     if (any(phi <= 0) || any(phi >=1)) return(Inf)
#     else
       return(-sum(d[[k]] * log(phi) + (d[[n]] - d[[k]]) * log(1 - phi)))
  }
}


