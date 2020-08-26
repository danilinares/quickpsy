#' Calculates the Akaike Information Criterion
#' \code{akaike} calculates the loglikelihoods.
akaike <- function(logliks) {

  logliks %>% transmute(aic = -2 * loglik + 2 * n_par)

}


