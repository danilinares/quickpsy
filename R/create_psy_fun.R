create_psy_fun <- function(psy_fun, guess, lapses) {
  if (psy_fun == 'cum_normal_fun') shape <- function(x,p) cum_normal_fun(x, p)
  if (psy_fun == 'logistic_fun') shape <- function(x,p) logistic_fun(x, p)

  function (x,p) {
    if (is.numeric(guess) && is.numeric(lapses)) {
      guess <- guess
      lapses <- lapses
      pshape <- p
    }
    if (is.logical(guess) && is.logical(lapses)){
      if (guess && lapses) {
        guess <- tail(p,2)[1]
        lapses <- tail(p,2)[2]
        pshape <- head(p,-2)
      }
    }
    if (is.numeric(guess) && is.logical(lapses)){
      if (lapses) {
        guess <- guess
        lapses <- tail(p,1)
        pshape <- head(p,-1)
      }
    }
    if (is.logical(guess) && is.numeric(lapses)){
      if (lapses) {
        guess <- tail(p,1)
        lapses <- lapses
        pshape <- head(p,-1)
      }
    }
    return(guess + (1 - guess- lapses) * shape(x, pshape))
  }
}
