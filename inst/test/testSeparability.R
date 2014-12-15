x<-c(1,2)
dd<-data.frame(x,k=c(40,70),r=c(60,30), n=c(100,100))
dd$y <- dd$k/dd$n

model<-glm(cbind(dd$k,dd$r)~x, family=binomial(probit))

nll<- function(p) {
  phi <- pnorm(x, p[[1]],p[[2]])
  -sum(dd$k * log(phi) + (dd$n - dd$k) * log(1 - phi))
}

z <- qnorm(dd$y)
coef <- lm(z~x)$coefficients
p1 <- -coef[[1]] / coef[[2]]
p2 <- 1 / coef[[2]]
para <- optim(c(p1,p2),nll)$p

xseq<-seq(0,3,.1)
yseq<-predict(model,data.frame(x=xseq),type='response')
yseq2<-pnorm(xseq, para[1],para[2])

curve<-data.frame(x=xseq,y=yseq, y2=yseq2)

ggplot(data=dd,aes(x=x,y=k/n))+geom_point()+
  geom_line(data=curve,aes(x=x,y=y))+
  geom_line(data=curve,aes(x=x,y=y2),color='red')
