#' handle_excep_quickpsy
#'
#' @export
handle_excep_quickpsy <- function(DE, pini, pini2) {
  if (DE && (is.null(pini) || is.null(pini2)))
    stop('DEoptim requires pini (vector with the lower bounds of the initial values of the parameters) and pini2 (vector with the upper bounds ofthe initial values of the parameters).', call. = F)
}
