#' \code{calculate_parini} calculate_parini
#' @keywords internal
#' @importFrom stats median qnorm lm
calculate_parini <- function(averages, funname, x, guess, lapses, grouping) {

  one_calculate_parini <- function(averages, funname, x, guess, lapses) {

    ntrials <- averages$n %>% first()
    x <- averages[[quo_name(x)]]
    y <- averages$prob

    if (is.numeric(guess) && is.numeric(lapses)) {
      gue <- guess
      lap  <- lapses
    }
    if (is.logical(guess) && is.logical(lapses)) {
      if (guess && lapses) {
        gue <- min(y)
        lap  <- 1 - max(y)
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
    datp <- data.frame(x = x, y01)


    ### Replacing 0s and/or 1s by 1 / (2 * n) and 1 - 1 / (2 * n)
    # where n is the number of trials
    datp <- datp %>%
      mutate(y01 = ifelse(y01 == 1, 1 - 1 / (2 * ntrials), y01)) %>%
      mutate(y01 = ifelse(y01 == 0, 1 / (2 * ntrials), y01))

    ### Eliminating probabilities outside (0,1)
    dat <- datp %>% filter(y01 > 0, y01 <1)


    if (length(dat$x) < 2) {
      ### When it is not possible to do the linear fit
      p1 <- median(datp$x)
      p2 <- (1 - gue - lap) / (max(datp$x)-min(datp$x))
    }
    else {
      ### Linear fit
      dat <- dat %>% mutate(z = qnorm(y01))
      coef <- lm(z ~ x, data = dat)$coefficients

      if (coef[[2]] == 0) { # checking that the slope is not zero
        p1 <- median(dat$x)
        p2 <- (1 - gue - lap) / (max(dat$x)-min(dat$x))
      }
      else {
        p1 <- -coef[[1]] / coef[[2]]
        p2 <- 1 / coef[[2]]
      }
    }

    if (funname  == "cum_normal_fun") {
      if (p2 < 0) p2 <- 1
    }
    if (funname == "logistic_fun") p2 <- 1 / p2
    if (funname  == "weibull_fun") {
      if (p1 < 0) p1 <- .Machine$double.eps
      if (p2 < 0) p2 <- 1
      p2 <- 1 / p2
    }

    if (gue == 0) gue <- -10
    else gue <- log(gue)
    if (lap == 0) lap <- -10
    else lap <- log(lap)

    if (is.numeric(guess) && is.numeric(lapses)) para <- c(p1, p2)
    if (is.logical(guess) && is.logical(lapses)) {
      if (guess && lapses) para <- c(p1, p2, gue, lap)
      if (!guess && !lapses) para <- c(p1, p2)
    }
    if (is.logical(guess) && is.numeric(lapses)) {
      if (guess) para <- c(p1, p2, gue)
      if (!guess) para <- c(p1, p2)
    }
    if (is.numeric(guess) && is.logical(lapses)) {
      if (lapses) para <- c(p1, p2, lap)
      if (!lapses) para <- c(p1, p2)
    }

    data.frame(parn = paste0('p', seq(1, length(para))), par = para)
  }

  averages %>%
    nest_by(.key = "averages") %>%
    summarise(one_calculate_parini(averages, funname, x, guess, lapses), .groups = "keep")

}



