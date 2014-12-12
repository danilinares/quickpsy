#' calculate_pini
#'
#' @export
calculate_pini <- function(d, x, k, n, psy_fun, guess, lapses) {
  ntrials <- unique(d[[n]])
  y <- d[[k]] / d[[n]]

  if (is.numeric(guess) && is.numeric(lapses)) {
    gue <- guess
    lap  <- lapses
  }
  if (is.logical(guess) && is.logical(lapses)) {
    if (guess && lapses) {
      gue <- min(y)
      lap  <- 1 - max(y)
    }
    if (!guess && !lapses) {
      gue <- 0
      lap  <- 0
    }
  }
  if (is.logical(guess) && is.numeric(lapses)) {
    lap <- lapses
    if (guess) gue <- min(y)
    if (!guess) gue <- 0
  }
  if (is.numeric(guess) && is.logical(lapses)) {
    gue <- guess
    if (lapses) lap <- 1 - max(y)
    if (!lapses) lap <- 0
  }

  ### Transforming y values to be closer to the range (0,1)
  y01 <- (y - gue) / (1 - gue - lap)
  datp <- data.frame(x = d[[x]], y01)

  ### Replacing 0s and/or 1s by 1 / (2 * n) and 1 - 1 / (2 * n) where n is the number of trials
  datp$y01[datp$y01 == 0] <- 1 / (2 * ntrials)
  datp$y01[datp$y01 == 1] <- 1 - 1 / (2 * ntrials)

  ### Eliminating probabilities outside (0,1)
  dat <- filter(datp, y01 > 0, y01 <1)

  ### Linear fit
  dat$z <- qnorm(dat$y01)
  coef <- lm(z~x, data = dat)$coefficients

  if (coef[[2]] == 0) { # checking that the slope is not zero
    p1 <- median(dat$x)
    p2 <- (1 - gue - lap) / (max(dat$x)-min(dat$x))
  }
  else {
    p1 <- -coef[[1]] / coef[[2]]
    p2 <- 1 / coef[[2]]
  }

  print(coef[[2]])
  if (is.numeric(guess) && is.numeric(lapses)) return(c(p1, p2))
  if (is.logical(guess) && is.logical(lapses)) {
    if (guess && lapses) return(c(p1, p2, gue, lap))
    if (!guess && !lapses) return(c(p1, p2))
  }
  if (is.logical(guess) && is.numeric(lapses)) {
    if (guess) return(c(p1, p2, gue))
    if (!guess) return(c(p1, p2))
  }
  if (is.numeric(guess) && is.logical(lapses)) {
    if (lapses) return(c(p1, p2, lap))
    if (!lapses) return(c(p1, p2))
  }
}

