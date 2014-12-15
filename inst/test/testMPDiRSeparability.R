x <- 10 ^ seq(-2.5, -.5, len = 6)
pr <- pweibull(x, 3, .075)
Trials <- 30
ny <- rbinom(length(pr), 30, pr)
nn <- 30 - ny


res <- glm(cbind(ny, nn) ~ log10(x), binomial(cloglog))

dat <- data.frame(x, pr, ny, nn, n = ny + nn, xlog = log10(x))

xseq <- seq(min(x), max(x), len=1000)
yseq <- predict(res, data.frame(x=xseq),type='response')

curve<-data.frame(xseq,yseq)

fit<-quickpsy(dat,x,ny,n, psyfun=weibull_fun)

ggplot()+
  geom_point(data = dat, aes(x= x, y=pr))+
  geom_line(data=curve,aes(x=xseq,y=yseq)) +
  geom_line(data=fit$curve,aes(x=x,y=y), color = 'red', lty=2) +
  scale_x_log10()

