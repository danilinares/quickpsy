library(dplyr)
library(ggplot2)
library(quickpsy)

dat <- read.table('inst/extdata/lopezmolinerlinares2006.txt', header = TRUE)
av <- dat %>% group_by(subject, phase, speed, cond) %>%
  summarise(ntrials = n(), nyes = sum(response), y = nyes / ntrials)

fit <- quickpsy(av, phase, nyes, ntrials, subject, speed, cond, cum_normal_fun,
                pini=c(.5,.3))

p <- ggplot(data=av,aes(x=phase,y=y, color= cond))+
  facet_grid(subject~speed) +
  geom_point()+
  geom_line(data=fit$curve)
p


