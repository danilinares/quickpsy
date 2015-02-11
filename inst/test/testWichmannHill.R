#### Wichmann and Hill 2001
library(gridExtra)

### sampling squema
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

### simulating data
dat <- merge(s, data.frame(n = c(20, 40, 80, 160)))

create_sim_dat <- function(d) {
  ypred <- weibull_fun(d$x, pgen)
  k <- rbinom(length(d$x), d$n, ypred)
  d %>% mutate(k)

}
simdat <- dat %>% group_by(n, s) %>% do(create_sim_dat(.))

fit <- quickpsy(simdat, x, k, n, within = .(s, n), fun = weibull_fun)
fit$para



left_join(data.frame(n = c(20, 40, 80, 160)),s)


s[[1]] <- data.frame(s = 1, x = inv_weibull_fun(c(.3, .4, .48, .52, .6, .7),pgen))
s[[2]] <- data.frame(s = 1, x = inv_weibull_fun(c(.3, .4, .48, .52, .6, .7),pgen))




x = PF([10 3 0 0],[.08 .18 .28 .7 .85 .99],'inverse');
case 6
x = PF([10 3 0 0],[.3 .4 .5 .6 .7 .99],'inverse');
case 7
x = PF([10 3 0 0],[.34 .44 .54 .8 .9 .98],'inverse');
