library(dplyr)
library(ggplot2)
library(quickpsy)

quickpsy <- function(d, x, k, n, random, within, between, psy_fun_name,
                     pini = NULL ,guess = 0, lapses = 0) {
  x <- deparse(substitute(x))
  k <- deparse(substitute(k))
  n <- deparse(substitute(n))
  grouping_var <- c()

  if (!missing(random)) {
    random <- deparse(substitute(random))
    grouping_var <- c(grouping_var, random)
  }
  if (!missing(within)) {
    within <- deparse(substitute(within))
    grouping_var <- c(grouping_var, within)
  }
  if (!missing(between)) {
    between <- deparse(substitute(between))
    grouping_var <- c(grouping_var, between)
  }

  d <- d %>% group_by_(.dots=grouping_var)

  fits <- d %>%
    do(fit=fit_psy(., x, k, n, psy_fun_name, pini, guess=guess, lapses=lapses))

  curve <-  plyr::ddply(fits,grouping_var, curve_psy)
  para <-  plyr::ddply(fits,grouping_var, para_psy)

  list(curve = curve , para = para)
}


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


