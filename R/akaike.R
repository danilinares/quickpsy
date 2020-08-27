#' Calculates the Akaike Information Criterion
#' \code{akaike} calculates the loglikelihoods.
#' @param logliks The \code{logliks} data frame from quickpsy.
#' @export
#' @importFrom rlang .data
akaike <- function(logliks) {

  logliks %>% transmute(aic = -2 * .data$loglik + 2 * .data$n_par)

}


