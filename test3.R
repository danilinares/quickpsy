
dat <- read.table('inst/extdata/lopezmolinerlinares2006.txt', header = TRUE)

av <- dat %>% group_by(phase, subject, cond, speed) %>%
  summarise(ntrials = n(), nyes = sum(response), y = nyes / ntrials)

av1 <- filter(av, subject == 'jlm', cond == 'auditory', speed == 15)
fit <- quickpsy(av1, phase, nyes, ntrials, psy_fun = cum_normal_fun,
                guess=T,lapses=T, DE=T, pini = c(-100,0,0,0),
                pini2 = c(100,10,1,1))

fit$para$p

model <- glm(cbind(av1$nyes,av1$ntrials-av1$nyes)~av1$phase,family=binomial(probit))
m <- -coef(model)[[1]]/coef(model)[[2]]
std <- 1/coef(model)[[2]]

xseq<-seq(-1,3,len=100)
yseq<-pnorm(xseq,m,std)
curve<-data.frame(phase=xseq,y=yseq)



p <- ggplot(data=av1,aes(x=phase,y=y))+
  geom_point()+
  geom_line(data=curve)+
  geom_line(data=fit$curve,color='red')+
  theme()
p

