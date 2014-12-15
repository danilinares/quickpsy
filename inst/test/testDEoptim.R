library(dplyr)
library(ggplot2)
library(DEoptim)

dat <- read.table('inst/extdata/lopezmolinerlinares2006.txt', header = TRUE)

av <- dat %>% filter(subject == 'dl') %>%
  group_by(phase,cond, speed) %>%
  summarise(ntrials = n(), nyes = sum(response), y = nyes / ntrials)

fit <- quickpsy(av, phase, nyes, ntrials,within = cond,
                between = speed, guess=T,lapses=T,
                psy_fun = logistic_fun,  pini = c(0,0,0,0),
                DE=T, pini2 = c(3,10,1,.2))

fit2 <- quickpsy(av, phase, nyes, ntrials,within = cond,
                between = speed, guess=T,lapses=T,
                psy_fun = logistic_fun)

p <- ggplot(data=av,aes(x=phase,y=y))+
  facet_grid(speed~cond) +
  geom_point()+
  geom_line(data=fit$curve, aes(color = 'DEoptim')) +
  geom_line(data=fit2$curve, aes(color = 'optim')) +
  ylim(-.5,1.5)
p

