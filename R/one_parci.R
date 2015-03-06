#' one_parci
#' @keywords internal
#' @export
one_parci <- function(d, ci) {
    parinf <- quantile(d$par, .5*(1 - ci))[[1]]
    parsup <- quantile(d$par, 1 - .5*(1 - ci))[[1]]
  data.frame(parinf, parsup)
}

