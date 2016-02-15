#' Creates the full negative log-likelihood function
#' \code{create_full_nll} Creates the full negative log-likelihood function
#' @keywords internal
#' @export
create_full_nll <- function(d, x, k, n, psyfunguesslapses){
  function(p) {
    phi <- psyfunguesslapses(d[[x]], p)
    phi[phi < .Machine$double.eps] <- .Machine$double.eps
    phi[phi > (1 - .Machine$double.eps)] <- 1 - .Machine$double.eps
    f <- data.frame(k = d[[k]], n = d[[n]]) %>% mutate(coef = lchoose(n,k))
    return(-sum(f$coef + d[[k]] * log(phi) + (d[[n]] - d[[k]]) * log(1 - phi)))

  }
}


