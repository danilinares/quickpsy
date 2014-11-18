#' handle_exceptions
#'
#' @export
handle_exceptions <- function(psy_fun, para, guess, lapses) {
  if (is.character(psy_fun))
    if (psy_fun == 'cum_normal_fun')
      if (is.logical(guess) && is.logical(lapses)) {
        if (guess && lapses) {
        if (para[3]<0) message('Warning: negative guess rate')
        if (para[4]<0) message('Warning: negative lapses rate')
        }
      }
}
