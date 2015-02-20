#' one_ypred
#'
#' @export
one_ypred <- function(d,log, groups, averages, x, psyfunguesslapses) {
  if (length(groups) != 0) averages <- dplyr::semi_join(averages, d, by = groups)
  xseq <- averages[[x]]
  yseq <- psyfunguesslapses(xseq, d$para)
  data.frame(x = xseq, ypred = yseq)
}



