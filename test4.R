library(dplyr)
library(ggplot2)

dat <- read.table('inst/extdata/lopezmolinerlinares2006.txt', header = TRUE)

av <- dat %>%
  group_by(phase) %>%
  summarise(ntrials = n(), nyes = sum(response), y = nyes / ntrials)

### providing the real function; there is not work for guesses and lapses
fit<-fit_psy(av, phase, nyes, ntrials, logistic_fun, c(.5,.5))
xseq <- seq(-1, 2, len = 100)
yseq <- fit$psy_fun(xseq, fit$para)
curve <- data.frame(x=xseq,y=yseq)
p <- ggplot(data=av,aes(x=phase,y=y))+
  geom_point()+
  geom_line(data=curve,aes(x=x,y=y))
p

### providing the label function with fixed guesses and lapses
fit<-fit_psy(av, phase, nyes, ntrials, 'cum_normal_fun', c(.5,.5),.25,.25)
xseq <- seq(-1, 2, len = 100)
yseq <- fit$psy_fun(xseq, fit$para)
curve <- data.frame(x=xseq,y=yseq)
p <- ggplot(data=av,aes(x=phase,y=y))+
  geom_point()+
  geom_line(data=curve,aes(x=x,y=y))
p


### providing the label function with  guesses and lapses as parameters
fit<-fit_psy(av, phase, nyes, ntrials, 'cum_normal_fun', c(.5,.5,.1,.1),T,T)
xseq <- seq(-1, 2, len = 100)
yseq <- fit$psy_fun(xseq, fit$para)
curve <- data.frame(x=xseq,y=yseq)
p <- ggplot(data=av,aes(x=phase,y=y))+
  geom_point()+
  geom_line(data=curve,aes(x=x,y=y))
p


### providing the label function with  guesses as parameters and lapses fixed
fit<-fit_psy(av, phase, nyes, ntrials, 'logistic_fun', c(.5,.5,.1),T,.25)
xseq <- seq(-1, 2, len = 100)
yseq <- fit$psy_fun(xseq, fit$para)
curve <- data.frame(x=xseq,y=yseq)
p <- ggplot(data=av,aes(x=phase,y=y))+
  geom_point()+
  geom_line(data=curve,aes(x=x,y=y))
p

### providing the label function with  fix guesses and lapses as parameter
fit<-fit_psy(av, phase, nyes, ntrials, 'cum_normal_fun', c(.5,.5,.25,.1),.25,T)
xseq <- seq(-1, 2, len = 100)
yseq <- fit$psy_fun(xseq, fit$para)
curve <- data.frame(x=xseq,y=yseq)
p <- ggplot(data=av,aes(x=phase,y=y))+
  geom_point()+
  geom_line(data=curve,aes(x=x,y=y))
p




