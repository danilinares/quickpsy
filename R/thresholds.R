#' thresholds
#'
#' @export
thresholds <- function(fitsGroups, threprob) {
  if (missing(threprob)) threprob <- fitsGroups$threprob
  plyr::ddply(fitsGroups$fits, fitsGroups$grouping_var, function(d)
    thre_psy(d, threprob))
}
