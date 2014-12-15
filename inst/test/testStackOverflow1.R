x <- c(-0.250, -0.056,  0.137,  0.331,  0.525,  0.719,  0.912,  1.100,  1.300)
k <- c(0, 0, 5, 11, 12, 12, 12, 12, 12)
n <- c(12, 12, 12, 12, 12, 12, 12, 12, 12)

nll <- function(p) {
  phi <- pnorm(x, p[1], p[2])
  phi[phi < .Machine$double.eps] <- .Machine$double.eps
  phi[phi > (1 - .Machine$double.eps)] <- 1 - .Machine$double.eps
  -sum(k * log(phi) + (n - k) * log(1 - phi))
}

para<- optim(c(0.1, 0.1), nll)$par

xseq <- seq(-.5, 1.5, len = 100)
yseq <- pnorm(xseq, para[1],para[2])
curve <- data.frame(xseq, yseq)

dat <- data.frame(x, k, n)

library(ggplot2)
ggplot(dat,aes(x = x, y = k / n)) +
  geom_point()+
  geom_line(data = curve, aes(x = xseq, y = yseq))




