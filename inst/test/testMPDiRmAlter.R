library(psyphy)

ecc2$LContr <- log10(ecc2$Contr)

ecc2 <- ecc2 %>% mutate(n = Correct +Incorrect, y= Correct/ n)

s1 <- filter(ecc2, Size == 20.6, task == 'DET')
s2 <- filter(ecc2, Size == 20.6, task == 'ID')

m1 <- glm(cbind(Correct, Incorrect) ~ LContr,
          family = binomial(mafc.probit(4)), data = s1)

m2 <- glm(cbind(Correct, Incorrect) ~ LContr,
          family = binomial(mafc.probit(4)), data = s2)

xseq <- seq(min(s1$LContr)-1, max(s1$LContr), len = 100)
yseq1 <- predict(m1, data.frame(LContr = xseq), type = 'response')
yseq2 <- predict(m2, data.frame(LContr = xseq), type = 'response')

curve1<-data.frame(xseq=10^(xseq),yseq=yseq1, task='DET')
curve2<-data.frame(xseq=10^(xseq),yseq=yseq2, task='ID')
curve<-rbind(curve1,curve2)

### quickpsy

fit<-quickpsy(rbind(s1,s2),within=.(task),Contr,Correct,n,log=T, guess=.25)


ggplot()+
  geom_point(data=s1, aes(x=Contr,y=y, color=task))+
  geom_point(data=s2, aes(x=Contr,y=y, color=task))+
  geom_line(data=curve,aes(x=xseq,y=yseq,color=task),alpha=.25,lwd=4)+
  geom_line(data=fit$curve,aes(x=Contr,y=y,color=task), lty=2)+
  scale_y_continuous(breaks=seq(0,1,.25))

