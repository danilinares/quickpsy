#' one_fit
#'
#' @export
one_fit <- function(d, x, k, n, psyfunguesslapses, psyfun,
                    pini, guess, lapses, DE, pini2) {

  if (psyfun %in% names(get_functions()))
    if (is.null(pini)) pini <- calculate_pini(d, x, k, n, guess, lapses)

  param <- fit_main(d, x, k, n, psyfunguesslapses, pini, DE, pini2)

  if (psyfun %in% names(get_functions()))
    if (is.logical(guess) && is.logical(lapses)) {
      if (guess && lapses) {
        if (param[3]<0) message('Warning: negative guess rate')
        if (param[4]<0) message('Warning: negative lapses rate')
      }
    }

  data.frame(param)
}
