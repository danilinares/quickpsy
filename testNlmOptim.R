### one data frame works with
nll<- function(p) {
  phi <- pnorm(d1$phase, p[[1]],p[[2]])
  -sum(d1$nyes * log(phi) + (d1$ntrials - d1$nyes) * log(1 - phi))
}

d1<- filter(av, subject=='dl', speed == 3, cond == 'self' )

#probit
d1p <- filter(d1, y > 0, y <1)
d1p$z <- qnorm(d1p$y)
coef <- lm(d1p$z~d1p$phase)$coefficients
p1 <- -coef[[1]] / coef[[2]]
p2 <- 1 / coef[[2]]
para <- optim(c(p1,p2),nll)$p
para2 <- nlm(nll,c(p1,p2))$est

xseq<-seq(-1,2,.01)
yseq<-pnorm(xseq, para2[1],para2[2])
curve<-data.frame(xseq,yseq)

ggplot(data=d1,aes(x=phase,y=y))+geom_point()+geom_line(data=curve,aes(x=xseq,y=yseq))

