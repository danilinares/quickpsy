library(dplyr)
library(ggplot2)
library(modelfree)
library(quickpsy)

data("01_Miranda")
x <- example01$x
r = example01$r
m = example01$m

example01 <- example01 %>% mutate(y = r / m)

fit<-fit_psy(example01, x, y, m, 'cum_normal_fun')



xseq <- seq(-1, 2, len = 100)
yseq <- fit$psy_fun(xseq, fit$para)
curve <- data.frame(x=xseq,y=yseq)
p <- ggplot(data=av,aes(x=phase,y=y))+
  geom_point()+
  geom_line(data=curve,aes(x=x,y=y))
p

ggplot(example01, aes(x = x, y = y))+
  geom_point()
