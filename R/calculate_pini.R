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

  y01 <- (y - gue) / (1 - gue - lap)
  datp <- data.frame(x = d[[x]], y01)
  dat <- filter(datp, y01 > 0, y01 <1) # Eliminating probabilities outside (0,1)
  if (nrow(dat) < 2) {
    dat <- filter(datp, y01 >= 0, y01 <= 1) # Eliminating probabilities outside [0,1]
    if (nrow(dat) < 2) stop('Initial values for the parameters cannot be calculated. Try to assign initial values manually', call. = F)
    else {
      warning('To calculate the initial values of the parameters 0s and/or 1s are replaced by 1 / (2 * n) and 1 - 1 / (2 * n) where n is the number of trials', call. = F)
      dat$y01[dat$y01 == 0] <- 1 / (2 * ntrials)
      dat$y01[dat$y01 == 1] <- 1 - 1 / (2 * ntrials)
    }
  }

  dat$z <- qnorm(dat$y01)
  coef <- lm(z~x, data = dat)$coefficients
  p1 <- -coef[[1]] / coef[[2]]
  p2 <- 1 / coef[[2]]

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

