#' Creates the psychometric function
#' \code{create_psy_fun} creates the psychometric function
#' @export
#' @param psy_fun The shape of the function without guesses and lapses.
#' @param guess Value indicating the guess rate (leftward asymptote) \eqn{\gamma}
#' (default is 0). If \code{TRUE}, the guess rate is estimated as the i + 1 parameter where
#' i corresponds to the number of parameters of \code{fun}. If, for
#' example, \code{fun} is a predefined shape with parameters p1 and p2,
#' then the guess rate corresponds to parameter p3.
#' @param lapses Value indicating the lapse rate (rightward asymptote) \eqn{\lambda}
#' (default is 0). If \code{TRUE}, the lapse rate is estimated as the i + 1 parameter where
#' i corresponds to the number of parameters of \code{fun} plus one if
#' the guess rate is estimated. If, for example, \code{fun} is a
#' predefined shape with parameters p1 and p2,
#' then the lapse rate corresponds to parameter p3. If the guess rate is also
#' estimated, p3 will be the guess rate and p4 the lapse rate.
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
