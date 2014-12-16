#' curves
#'
#' @export
curves <- function(fitsGroups, xmin = NULL, xmax = NULL, log = F) {
  plyr::ddply(fitsGroups$fits, fitsGroups$grouping_var,
              function(d) curve_psy(d, xmin, xmax, log))
}
