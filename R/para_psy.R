#' para_psy
#'
#' @export
para_psy <- function(l) {
  f <- l$fit[[1]]
  data.frame(p=f$para)
}
