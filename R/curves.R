#' curves
#'
#' @export
curves <- function(fitsGroups, xmin, xmax, log) {
  plyr::ddply(fitsGroups$fits, fitsGroups$grouping_var,
              function(d) curve_psy(d, xmin, xmax, log))
}
