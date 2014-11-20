library(dplyr)
library(ggplot2)
library(quickpsy)

dat <- read.table('inst/extdata/lopezmolinerlinares2006.txt', header = TRUE)
av <- dat %>%
  group_by(phase, speed) %>%
  summarise(ntrials = n(), nyes = sum(response), y = nyes / ntrials)

av %>% group_by(speed) %>% mutate( hh = nyes)

fit_psy <- function(d, x, k, n, psy_fun_name, pini = NULL, guess, lapses) {
  #   x <- deparse(substitute(x))
  #   k <- deparse(substitute(k))
  #   n <- deparse(substitute(n))
  if (is.character(psy_fun_name)) {
    pini <- calculate_pini(d, x, k, n, psy_fun_name, guess, lapses)

    psy_fun <- create_psy_fun(psy_fun_name, guess, lapses)
  }
  else if (is.function(psy_fun_name)){
    psy_fun <- psy_fun_name
  }
  cat('Initial parameters:',pini)
  para <- fit_main(d, x, k, n, psy_fun, pini)
  handle_exceptions(psy_fun_name, para, guess, lapses)
  list(para = para,
       psy_fun = psy_fun)
}
#fit_psy(av, 'phase', 'nyes', 'ntrials', 'cum_normal_fun',guess=T,lapses=T)

quickpsy <- function(d, x, k, n, psy_fun_name, pini = NULL, guess, lapses) {
#   x <- lazy(x)
#   k <- lazy(k)
#   n <- lazy(n)
#   x <- deparse(substitute(x))
#   k <- deparse(substitute(k))
#   n <- deparse(substitute(n))
  d %>% do(mod=fit_psy(., x = x, k = k, n =n , psy_fun_name, pini = NULL, guess, lapses))
}
fit<-quickpsy(av, phase, nyes, ntrials, 'cum_normal_fun',guess=T,lapses=T)


fit


#change by joan

#Second change by me!!

### providing the label function with  guesses and lapses as parameters
fit<-fit_psy(av, phase, nyes, ntrials, 'cum_normal_fun',guess=T,lapses=T)
xseq <- seq(-1, 2, len = 100)
yseq <- fit$psy_fun(xseq, fit$para)
curve <- data.frame(x=xseq,y=yseq)
p <- ggplot(data=av,aes(x=phase,y=y,color=factor(speed)))+
  geom_point()
 # geom_line(data=curve,aes(x=x,y=y))
p

### providing the label function with fixed guesses and lapses
fit<-fit_psy(av, phase, nyes, ntrials, 'cum_normal_fun', guess = 0, lapses = 0)
xseq <- seq(-1, 2, len = 100)
yseq <- fit$psy_fun(xseq, fit$para)
curve <- data.frame(x=xseq,y=yseq)
p <- ggplot(data=av,aes(x=phase,y=y))+
  geom_point()+
  geom_line(data=curve,aes(x=x,y=y))
p

### providing the label function with fixed guesses and lapses fixex to zero using FALSE
fit<-fit_psy(av, phase, nyes, ntrials, 'cum_normal_fun', guess = F, lapses = F)
xseq <- seq(-1, 2, len = 100)
yseq <- fit$psy_fun(xseq, fit$para)
curve <- data.frame(x=xseq,y=yseq)
p <- ggplot(data=av,aes(x=phase,y=y))+
  geom_point()+
  geom_line(data=curve,aes(x=x,y=y))
p

### providing the label function with  guesses as parameters and lapses fixed
fit<-fit_psy(av, phase, nyes, ntrials, 'cum_normal_fun',guess = T, lapses = .25)
xseq <- seq(-1, 2, len = 100)
yseq <- fit$psy_fun(xseq, fit$para)
curve <- data.frame(x=xseq,y=yseq)
p <- ggplot(data=av,aes(x=phase,y=y))+
  geom_point()+
  geom_line(data=curve,aes(x=x,y=y))
p

### providing the label function with  guess=0 using FALSE as parameters and lapses F
fit<-fit_psy(av, phase, nyes, ntrials, 'cum_normal_fun',guess = F, lapses = .25)
xseq <- seq(-1, 2, len = 100)
yseq <- fit$psy_fun(xseq, fit$para)
curve <- data.frame(x=xseq,y=yseq)
p <- ggplot(data=av,aes(x=phase,y=y))+
  geom_point()+
  geom_line(data=curve,aes(x=x,y=y))
p

### providing the label function with fix guesses and lapses as parameter
fit<-fit_psy(av, phase, nyes, ntrials, 'cum_normal_fun',guess = .25, lapses = T)
xseq <- seq(-1, 2, len = 100)
yseq <- fit$psy_fun(xseq, fit$para)
curve <- data.frame(x=xseq,y=yseq)
p <- ggplot(data=av,aes(x=phase,y=y))+
  geom_point()+
  geom_line(data=curve,aes(x=x,y=y))
p

### providing the label function with fix guesses and lapses=0 using FALSE
fit<-fit_psy(av, phase, nyes, ntrials, 'cum_normal_fun',guess = .25, lapses = F)
xseq <- seq(-1, 2, len = 100)
yseq <- fit$psy_fun(xseq, fit$para)
curve <- data.frame(x=xseq,y=yseq)
p <- ggplot(data=av,aes(x=phase,y=y))+
  geom_point()+
  geom_line(data=curve,aes(x=x,y=y))
p

### providing the real function
fit<-fit_psy(av, phase, nyes, ntrials, logistic_fun, c(.5,.5))
xseq <- seq(-1, 2, len = 100)
yseq <- fit$psy_fun(xseq, fit$para)
curve <- data.frame(x=xseq,y=yseq)
p <- ggplot(data=av,aes(x=phase,y=y))+
  geom_point()+
  geom_line(data=curve,aes(x=x,y=y))
p


### providing the real function: two cumulative normal
av2 <- av
av2$phase <- -.5*av2$phase+3
avt<- rbind(av,av2)
twocum<- function(x,p) cum_normal_fun(x,c(p[1],p[2]))-cum_normal_fun(x,c(p[3],p[4]))

fit<-fit_psy(avt, phase, nyes, ntrials, twocum, c(0,1,3,1))
xseq <- seq(-1, 4, len = 100)
yseq <- fit$psy_fun(xseq, fit$para)
curve <- data.frame(x=xseq,y=yseq)
p <- ggplot(data=avt,aes(x=phase,y=y))+
  geom_point()+
  geom_line(data=curve,aes(x=x,y=y))
p

### just two points
twopointd <- data.frame(phase = c(0,3), nyes = c(50,60), y = c(.5, .6), ntrials = c(100,100))
fit<-fit_psy(twopointd, phase, nyes, ntrials, 'cum_normal_fun', guess = 0, lapses = 0)
xseq <- seq(-30, 30, len = 100)
yseq <- fit$psy_fun(xseq, fit$para)
curve <- data.frame(x=xseq,y=yseq)
p <- ggplot(data=twopointd,aes(x=phase,y=y))+
  geom_point()+
  geom_line(data=curve,aes(x=x,y=y))
p


### Errors: One point data frame.
onepointd <- data.frame(phase = 0, nyes = 100, ntrials = 100)
fit<-fit_psy(onepointd, phase, nyes, ntrials, 'cum_normal_fun', guess = 0, lapses = 0)

### just two points: zero and one
twopointd <- data.frame(phase = c(0,3), nyes = c(0,100), y = c(0, 1), ntrials = c(100,100))
fit<-fit_psy(twopointd, phase, nyes, ntrials, 'cum_normal_fun', guess = 0, lapses = 0)
xseq <- seq(-1, 4, len = 100)
yseq <- fit$psy_fun(xseq, fit$para)
curve <- data.frame(x=xseq,y=yseq)
p <- ggplot(data=twopointd,aes(x=phase,y=y))+
  geom_point()+
  geom_line(data=curve,aes(x=x,y=y))
p







