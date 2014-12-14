x <- c(-0.200,  0.138,  0.475,  0.812,  1.150,  1.488,  1.825,  2.162,  2.500)
k <- c(0, 1, 1, 3, 8, 8, 8, 8, 8)
n <- c(8, 8, 8, 8, 8, 8, 8, 8, 8)

nll <- function(p) {
  phi <- pnorm(x, p[1], p[2])
  phi[phi < .Machine$double.eps] <- .Machine$double.eps
  phi[phi > (1 - .Machine$double.eps)] <- 1 - .Machine$double.eps
  -sum(k * log(phi) + (n - k) * log(1 - phi))
}

para<- optim(c(0.1, 0.1), nll)$par
para
xseq <- seq(-.5, 2.5, len = 100)
yseq <- pnorm(xseq, para[1],para[2])
curve <- data.frame(xseq, yseq)

nllgl <- function(p) {
  phi <- p[3] + (1 - p[3] - p[4]) * pnorm(x, p[1], p[2])
  phi[phi < .Machine$double.eps] <- .Machine$double.eps
  phi[phi > (1 - .Machine$double.eps)] <- 1 - .Machine$double.eps
  -sum(k * log(phi) + (n - k) * log(1 - phi))
}

paragl<- optim(c(0.1, 0.1, 0, 0), nllgl)$par
paragl

yseqgl <- paragl[3] + (1 - paragl[3] - paragl[4]) * pnorm(xseq, paragl[1],paragl[2])
curvegl <- data.frame(xseq, yseq=yseqgl)

#mod<-DEoptim(nllgl,lower=-100,upper=100,,DEoptim.control(itermax=B))$optim
mod <- DEoptim(nllgl,lower=c(min(x),0,0,0),upper=c(max(x),1,1,.5))$optim
paradeoptim<-mod$bestmem

yseqgldeoptim <- paradeoptim[3] +
  (1 - paradeoptim[3] - paradeoptim[4]) * pnorm(xseq, paradeoptim[1],paradeoptim[2])
curvegldeoptim <- data.frame(xseq, yseq=yseqgldeoptim)


ggplot(dat,aes(x = x, y = k / n)) +
  geom_point()+
  geom_line(data = curve, aes(x = xseq, y = yseq, color = 'no guess and lapses')) +
 geom_line(data = curvegl, aes(x = xseq, y = yseq, color = 'guess and lapses')) +
  geom_line(data = curvegldeoptim, aes(x = xseq, y = yseq, color = 'de optim'))



