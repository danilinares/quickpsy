library(dplyr)
library(ggplot2)
library(quickpsy)



dat <- read.table('inst/extdata/lopezmolinerlinares2006.txt', header = TRUE)

av <- dat %>% group_by(phase) %>%
  summarise(ntrials = n(), nyes = sum(response), y = nyes / ntrials)
fit <- quickpsy(av, phase, nyes, ntrials, psy_fun = cum_normal_fun, guess=T,lapses=T)

p <- ggplot(data=av,aes(x=phase,y=y))+
  geom_point()+
  geom_line(data=fit$curve)
p


av <- dat %>% group_by(phase, subject) %>%
  summarise(ntrials = n(), nyes = sum(response), y = nyes / ntrials)
fit <- quickpsy(av, phase, nyes, ntrials, random = subject,
                psy_fun = cum_normal_fun)

p <- ggplot(data=av,aes(x=phase,y=y))+
  facet_wrap(~subject) +
  geom_point()+
  geom_line(data=fit$curve)
p


av <- dat %>% group_by(phase, subject, cond) %>%
  summarise(ntrials = n(), nyes = sum(response), y = nyes / ntrials)
fit <- quickpsy(av, phase, nyes, ntrials, random = subject, within = cond,
                psy_fun = cum_normal_fun, pini=c(.5,.3))

p <- ggplot(data=av,aes(x=phase,y=y, color=cond))+
  facet_wrap(~subject) +
  geom_point()+
  geom_line(data=fit$curve)
p


av <- dat %>% group_by(phase, subject, cond, speed) %>%
  summarise(ntrials = n(), nyes = sum(response), y = nyes / ntrials)

fit <- quickpsy(av, phase, nyes, ntrials, random = subject, within = cond,
                between = speed,
                psy_fun = cum_normal_fun)

p <- ggplot(data=av,aes(x=phase,y=y, color=cond))+
  facet_grid(speed~subject) +
  geom_point()+
  geom_line(data=fit$curve)
p


av1 <- filter(av, subject == 'dl', cond == 'self', speed == 3)
fit <- quickpsy(av1, phase, nyes, ntrials, psy_fun = cum_normal_fun)

p <- ggplot(data=av1,aes(x=phase,y=y))+
  geom_point()+
  geom_line(data=fit$curve)
p




