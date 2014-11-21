library(dplyr)
library(ggplot2)
library(modelfree)
library(quickpsy)

data("01_Miranda")
x <- example01$x
r = example01$r
m = example01$m

example01 <- example01 %>% mutate(y = r / m)


val <- binomfit_lims( r, m, x, link = "probit" )
numxfit <- 999
xseq <- seq(0, 1.5, 0.01)
yseq <- predict( val$fit, data.frame( x = xseq ), type = "response" )
curve <- data.frame(xfit, pfit)

p <- ggplot(data = example01, aes(x = x, y = y))+
  geom_point()+
  geom_line(data = curve, aes(x = xfit, y = pfit))
p

