#### Wichmann and Hill 2001
library(gridExtra)

### sampling squemas
pgen <- c(10, 3)
create_xs <- function(i, f) data.frame(s = i,  y = f,
                                       x = inv_weibull_fun(f,pgen))
s <- list()
s[[1]] <- create_xs(1, c(.3, .4, .48, .52, .6, .7))
s[[2]] <- create_xs(2, c(.1, .3, .4, .6, .7, .9))
s[[3]] <- create_xs(3, c(.3, .44, .7, .8, .9, .98))
s[[4]] <- create_xs(4, c(.1, .2, .3, .4, .5, .6))
s[[5]] <- create_xs(5, c(.08, .18, .28, .7, .85, .99))
s[[6]] <- create_xs(6, c(.3, .4, .5, .6, .7, .99))
s[[7]] <- create_xs(7, c(.34, .44, .54, .8, .9, .98))
s <- do.call('rbind', s)

xseq <- seq(min(s$x), max(s$x), length = 100)
yseq <- weibull_fun(xseq, pgen)
curve <- data.frame(x = xseq, y = yseq)

pSampling <- ggplot(s, aes(x = x, y = s, color = factor(s))) +
  geom_point() + geom_line() + theme(legend.position = 'top')

pSampling2 <- ggplot() + facet_wrap(~s, ncol = 2) +
  geom_point(data = s, aes(x = x, y = y, color = factor(s))) +
  geom_line(data = curve,aes(x = x, y = y)) +
  theme(legend.position = 'none')

grid.arrange(pSampling, pSampling2, ncol = 2)

### creating fake samples
create_sim_dat <- function(d) {
  ypred <- .5 + (1 - .5 - d$lam) * weibull_fun(d$x, pgen)
  k <- rbinom(length(d$x), d$n, ypred)
  data.frame(x=d$x, k = k, n =d$n , y = k/d$n)
}

simdat <- merge(s, expand.grid(n = 160, sample = 1:40, lam = seq(0,.05, .01))) %>%
  group_by(s, lam, sample) %>% do(create_sim_dat(.))

ggplot(simdat) + facet_grid(s~lam) + geom_point(aes(x=x,y=y,group=sample))


### sin lapsos hay bias
fitNoLapses <- quickpsy(simdat, x, k, n, within = .(s, lam, sample),
                        fun = weibull_fun, bootstrap = 'none', guess =.5)


paraNoLapses <- fitNoLapses$para %>% group_by(s, lam, paran) %>%
  summarise (par = mean(para))

ggplot(paraNoLapses) +
facet_wrap(~paran, scales = 'free') +
  geom_point(aes(x = lam, y = par, color = factor(s) ))

threNoLapses <- fitNoLapses$thresholds %>% group_by(s, lam) %>%
  summarise (threshold = mean(thre)) %>% mutate(model = 'No lapsos')

ggplot(threNoLapses) +
  geom_hline(aes(yintercept =  inv_weibull_fun(.5,pgen)), lty=2) +
  geom_point(aes(x = lam, y = threshold, color = factor(s)), size = 4)

### con lapsos free es una porqueria porque lambda es muy grande
fitLapses <- quickpsy(simdat, x, k, n, within = .(s,lam, sample), lapses = T,
                      fun = weibull_fun, bootstrap = 'none', guess =.5)

paraLapses <- fitLapses$para %>% group_by(s, lam, paran) %>%
  summarise (par = mean(para))

ggplot(paraLapses) +
  facet_wrap(~paran, scales = 'free') +
  geom_point(aes(x = lam, y = par, color = factor(s) ))

### con lapsos entre 0 y 0.05
simdat <- merge(s, expand.grid(n = 160, sample = 1:1, lam = seq(0,.05, .01))) %>%
  group_by(s, lam, sample) %>% do(create_sim_dat(.))
fitLapses05 <- quickpsy(simdat, x, k, n, within = .(s,lam, sample), lapses = T,
                      fun = weibull_fun, bootstrap = 'none', guess =.5,
                      pini=list(c(3,20),c(1,8),c(0,.05)))

plotcurves(fitLapses05, color = sample)+geom_vline(xintercept=10)

paraLapses05 <- fitLapses05$para %>% group_by(s, lam, paran) %>%
  summarise (par = mean(para))

ggplot(paraLapses05) +
  facet_wrap(~paran,scales='free') +
  geom_point(aes(x = lam, y = par, color = factor(s) ))

threLapses05 <- fitLapses05$thresholds %>% group_by(s, lam) %>%
  summarise (threshold = mean(thre))%>% mutate(model = 'Lapsos entre 0 y .05')

ggplot(threLapses05) +
  geom_hline(aes(yintercept =  inv_weibull_fun(.5,pgen)), lty=2) +
  geom_point(aes(x = lam, y = threshold, color = factor(s)), size = 4)

thre <- rbind(threNoLapses, threLapses05)
pthre <- ggplot(thre) + facet_wrap(~s)+
  geom_hline(aes(yintercept =  inv_weibull_fun(.5,pgen)), lty=2) +
  geom_point(aes(x = lam, y = threshold, color = model), size = 4)
pthre

### least squares

simdat <- merge(s, expand.grid(n = 160, sample = 1:3, lam = seq(0,.05, .01))) %>%
  group_by(s, lam, sample) %>% do(create_sim_dat(.))

ggplot(simdat) + facet_grid(s~lam) + geom_point(aes(x=x,y=y,color=factor(sample)))


paraLS <- function(d) {
  ls <- function(p, d){
    ypred <- .5 + .5*weibull_fun(d$x, p)
    sum((d$y-ypred)^2)
  }
  para <- optim(c(10,2), ls, d=d)$p
  data.frame(para, paran = c('p1','p2'))
}

fitLS <- simdat %>% group_by(s, lam, sample) %>% do(paraLS(.))

paraLS <- fitLS %>% group_by(s, lam, paran) %>%
  summarise (par = mean(para),
             paramin = quantile(para, .025),
             paramax = quantile(para, .975))

threLS <- paraLS %>% mutate(thre = inv_weibull_fun(.5,par))

myfun<-function(x) {
  xseq<-seq(0,20,0.1)
  yseq<-.5+.5*weibull_fun(xseq,x$par)
  data.frame(xseq,yseq)
}
curveLS <- paraLS %>% do(myfun(.))

ggplot(simdat) + facet_grid(s~lam) + geom_point(aes(x=x,y=y,color=factor(sample)))+
  geom_line(data=curveLS,aes(x=xseq,y=yseq,color=factor(sample)))


ggplot(paraLS) +
  facet_grid(paran~s, scales = 'free') +
  geom_pointrange(aes(x = lam, y = par, ymin=paramin,ymax=paramax ))


ggplot(threLS) +
  facet_grid(.~s, scales = 'free') +
  geom_point(aes(x = lam, y = thre ))






ggplot(paraLapses) +
  facet_grid(paran~s, scales = 'free') +
  geom_pointrange(aes(x = lam, y = par, ymin=paramin,ymax=paramax ))


### lapses entre 0 y 0.05
fitNoLapses <- quickpsy(simdat, x, k, n, within = .(s, lam, sample),
                        fun = weibull_fun, bootstrap = 'none', guess =.5)


### parameters lapses = T
fitLapses <- quickpsy(simdat, x, k, n, within = .(s,lam, sample), lapses = T,
                        fun = weibull_fun, bootstrap = 'none', guess =.5)

paraLapses <- fitLapses$para %>% group_by(s, lam, paran) %>%
  summarise (par = mean(para),
             paramin = quantile(para, .025),
             paramax = quantile(para, .975))

ggplot(paraLapses) +
  facet_grid(paran~s, scales = 'free') +
  geom_pointrange(aes(x = lam, y = par, ymin=paramin,ymax=paramax ))

### thresholds  lapses = T
threLapses <- fitLapses$thresholds %>% group_by(s, lam) %>%
  summarise (threshold = mean(thre),
             thresholdmin = quantile(thre, .025),
             thresholdmax = quantile(thre, .975))

ggplot(threLapses) +
  facet_wrap(~s) +
  geom_hline(aes(yintercept =  inv_weibull_fun(.5,pgen)), lty=2) +
  geom_pointrange(aes(x = lam, y = threshold,
                      ymin=thresholdmin,ymax=thresholdmax ))







averagesThresholds <- fit$thre %>% group_by(s, lam) %>%
  summarise (par = mean(thre),
             paramin = quantile(thre, .025),
             paramax = quantile(thre, .975))

ggplot(averagesThresholds) +
  facet_wrap(~s) +
  geom_pointrange(aes(x = lam, y = par, ymin=paramin,ymax=paramax ))

p2


plotpara(fit) + geom_hline(data = data.frame(pgen), aes(yintercept=pgen))

fit2 <- quickpsy(simdat, x, k, n, within = .(s,lam, sample), fun =  weibull_fun,
                lapses = T,  bootstrap = 'none')

plotpara(fit2) + geom_hline(data = data.frame(pgen), aes(yintercept=pgen))




