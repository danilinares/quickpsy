n <- 100
x <- c(.2, .4, .6, .8, 1) #luminance
k <- c(59, 56, 69, 84, 96) # number of times that the observer reports that can see the stimulus
y <- k/n

y01 <- (y-.5)/.5
z <- qnorm(y01)

dat <- data.frame(x, k, y, z)

para <- lm(z~x, data=dat)$coefficients

p1<- -para[[1]]/para[[2]]

p2<- 1/para[[2]]

xseq<-seq(0,1,.01)
yseq<-0.5+0.5*pnorm(xseq,p1,p2)
yseq2<-0.5+0.5*pnorm(xseq,para[1],para[2])

curve<-data.frame(xseq,yseq,yseq2)

p<-ggplot()+
  geom_point(data=dat,aes(x=x,y=y))+
  geom_line(data=curve, aes(x=xseq,y=yseq))+
  geom_line(data=curve, aes(x=xseq,y=yseq2),color='red')
p

m <- 2
nll <- function(p) {
  phi <- 1/m + 1/m * pnorm(x, p[1], p[2])
  -sum(k * log(phi) + (n - k) * log(1 - phi))
}

para <- optim(c(.7,.7), nll)$p
para
