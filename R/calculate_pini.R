#' calculate_pini
#'
#' @export
calculate_pini <- function(d, x, k, n, psy_fun, guess, lapses) {
  y <- d[[k]] / d[[n]]

  if (is.numeric(guess) && is.numeric(lapses)) {
    y01 <- (y - guess) / (1 - guess - lapses)
    dat <- data.frame(x = d[[x]], y01)
    dat <- dat[dat$y01> 0 & dat$y01 < 1,]

    if (psy_fun == 'cum_normal_fun') {
      dat$z <- qnorm(dat$y01)
      coef <- lm(z~x, data = dat)$coefficients
      p1 <- -coef[[1]] / coef[[2]]
      p2 <- 1 / coef[[2]]
    }
    return(c(p1, p2))
  }
  if (is.logical(guess) && is.logical(lapses)) {
    y01 <- (y - 0) / (1 - 0 - 0)
    dat <- data.frame(x = d[[x]], y01)
    dat <- dat[dat$y01> 0 & dat$y01 < 1,]

    if (psy_fun == 'cum_normal_fun') {
      dat$z <- qnorm(dat$y01)
      coef <- lm(z~x, data = dat)$coefficients
      p1 <- -coef[[1]] / coef[[2]]
      p2 <- 1 / coef[[2]]
    }
    return(c(p1, p2, 0, 0))
  }

}
