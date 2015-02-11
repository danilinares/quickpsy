#' one_thresholdsci
#'
#' @export
one_thresholdsci <- function(d, ci, method) {
  if (method == 'percent') {
    threinf <- quantile(d$thre, .5*(1 - ci))[[1]]
    thresup <- quantile(d$thre, 1 - .5*(1 - ci))[[1]]
  }
  data.frame(threinf, thresup)
}

