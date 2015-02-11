#' one_paraci
#'
#' @export
one_paraci <- function(d, ci, method) {
  if (method == 'percent') {
    parainf <- quantile(d$para, .5*(1 - ci))[[1]]
    parasup <- quantile(d$para, 1 - .5*(1 - ci))[[1]]
  }
  data.frame(parainf, parasup)
}

