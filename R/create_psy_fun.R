#' Creates the psychometric function
#' \code{create_psy_fun} creates the psychometric function
#' @export
#' @importFrom utils tail head
create_psy_fun <- function(psy_fun, guess, lapses) {

  function (x, p) {
    if (is.numeric(guess) && is.numeric(lapses)) {
      guess <- guess
      lapses <- lapses
      pshape <- p
    }

    if (is.logical(guess) && is.logical(lapses)){
      if (guess && lapses) {
        guess <- exp(tail(p,2)[1])
        lapses <- exp(tail(p,2)[2])
        pshape <- head(p,-2)
      }
      if (!guess && !lapses) {
        guess <- 0
        lapses <- 0
        pshape <- p
      }
    }
    if (is.numeric(guess) && is.logical(lapses)){
      if (lapses) {
        guess <- guess
        lapses <- exp(tail(p,1))
        pshape <- head(p,-1)
      }
      if (!lapses) {
        guess <- guess
        lapses <- lapses
        pshape <- p
      }
    }
    if (is.logical(guess) && is.numeric(lapses)){
      if (guess) {
        guess <- exp(tail(p,1))
        lapses <- lapses
        pshape <- head(p,-1)
      }
      if (!guess) {
        guess <- guess
        lapses <- lapses
        pshape <- p
      }
    }

    guess + (1 - guess- lapses) * psy_fun(x, pshape)
  }
}
