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


x <- c(-0.250, -0.056,  0.137,  0.331,  0.525,  0.719,  0.912,  1.100,  1.300)
k <- c(0, 0, 5, 11, 12, 12, 12, 12, 12)
n <- c(12, 12, 12, 12, 12, 12, 12, 12, 12)









x <- c(-0.250, -0.056,  0.137,  0.331,  0.525,  0.719,  0.912,  1.100,  1.300)
k <- c(0, 0, 5, 11, 12, 12, 12, 12, 12)
n <- c(12, 12, 12, 12, 12, 12, 12, 12, 12)


nll <- function(p, d) {
  phi <- pnorm(x, p[1], p[2])
  -sum(k * log(phi) + (n - k) * log(1 - phi))
}

para<- optim(c(0.1, 0.1), nll)$par

xseq <- seq(-.5, 1.5, len = 100)
yseq <- pnorm(x,para.2[1],para.2[2])
curve <- data.frame(xseq, yseq)

dat <- data.frame(x, k, n)

library(ggplot2)
ggplot(dat,aes(x = x, y = k / n)) +
  geom_point()+
  geom_line(data = curve, aes(x = xseq, y = yseq))


nll <- function(p, d) {
  logphi1 <- pnorm(x, p[1], p[2], lower.tail = T, log.p = T)
  logphi2 <- pnorm(x, p[1], p[2], lower.tail = F, log.p = T)
  -sum(k * logphi1 + (n - k) * logphi2)
}
para<- optim(c(0.1, 0.1), nll)$par

MLEparameters2 <- optim(c(0.1, 0.1), nll2)$par

model<-glm(cbind(k,n-k)~x,family=binomial(probit))
model<-glm(cbind(k,n-k)~x,family=binomial(mafc.probit(2)))

m <- -coef(model)[[1]]/coef(model)[[2]]
std <- 1/coef(model)[[2]]

para<-(m,std)

xseq <- seq(-.5, 1.5, len = 100)
yseq <- pnorm(xseq, para[1],para[2])
curve <- data.frame(xseq, yseq)




x <- c(-0.250,  0.331,  0.525,  0.719,  0.912,  1.100,  1.300)
k <- c(0, 0,  12, 12, 12, 12, 12)
n <- c(12, 12, 12, 12, 12, 12, 12)



nll <- function(p) {
  phi   <- pnorm(x, p[1], p[2])
  eps   <- 1e-10
  terms <- ifelse(phi<eps | phi>1-eps, 0,k * log(phi) + (n - k) * log(1 - phi))
  -sum(terms)
}
par(mfrow=c(1,2))
para.1<- optim(c(1, 2), nll)$par
plot(x,k/n)
curve(pnorm(x,para.1[1],para.1[2]),add=T)

para.2<- optim(c(0.1, 0.1), nll)$par
plot(x,k/n)
curve(pnorm(x,para.2[1],para.2[2]),add=T)

