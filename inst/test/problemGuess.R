###############################################
library(dplyr)
library(ggplot2)
library(robustbase)

data <- qreadfiles('inst/extdata/linareslopezmolinerjohnston2007/',
                   obs = c('dl','ss','at'), exp = c('exp1'))

dat_dl_800_2 <- filter(data, obs == 'dl', INTERVAL == 800, EXC_F == 2)
#dat_dl_800_2 <- dat_dl_800_2 %>% filter(FASE < 410)

fitquickpsy <- quickpsy(dat_dl_800_2, FASE, RESP)
curvesquickpsy <- fitquickpsy$curves %>% mutate(fit = 'quickpsy')

fitquickpsylapses <- quickpsy(dat_dl_800_2, FASE, RESP, lapses = T)
curvesquickpsylapses <- fitquickpsylapses$curves %>% mutate(fit = 'quickpsy\nwith lapses')

fitglm <- glm(cbind(RESP, n - RESP) ~ FASE, data = fitquickpsy$averages,  family = binomial(probit))
xseq <- seq(0,500, length = 300)
yseq <- predict(fitglm, data.frame(FASE = xseq), type = 'response')
paraglm <- c(-coef(fitglm)[[1]] / coef(fitglm)[[2]], 1 / coef(fitglm)[[2]])
curvesglm <- data.frame(x = xseq, y = yseq) %>% mutate(fit = 'glm')

fitglmrob <- glmrob(cbind(RESP, n - RESP) ~ FASE, data = fitquickpsy$averages,  family = binomial(probit))
yseq <- predict(fitglmrob, data.frame(FASE = xseq), type = 'response')
curvesglmrob <- data.frame(x = xseq, y = yseq) %>% mutate(fit = 'glmrob')

leastSq <- function(p) {
  y <- fitquickpsy$averages$y
  ypred <- cum_normal_fun( fitquickpsy$averages$FASE, p)
  sum((y - ypred)^2)
}
para <- optim(c(100,10), leastSq)$par
yseq <- cum_normal_fun(xseq, para)
curvesLeastSq <- data_frame(x = xseq, y = yseq) %>% mutate(fit = 'least sq')

ggplot()+
  facet_wrap(~fit) +
  geom_line(data = curvesLeastSq, aes(x = x, y = y, color = fit)) +
  geom_line(data = curvesquickpsy, aes(x = x, y = y, color = fit)) +
  geom_line(data = curvesquickpsylapses, aes(x = x, y = y, color = fit)) +
  geom_line(data = curvesglm, aes(x = x, y = y, color = fit)) +
  geom_line(data = curvesglmrob, aes(x = x, y = y, color = fit)) +
  geom_point(data = fitquickpsy$averages, aes(x = FASE, y = y ))



fitquickpsywithlapsepar <- fitquickpsy
fitquickpsywithlapsepar$para <- head(fitquickpsylapses$para,2)

loglik(fitquickpsy)$loglik > loglik(fitquickpsywithlapsepar)$loglik




fit <- quickpsy(dat_dl_800_2, FASE, RESP, guess = F, pini=c(0,0),pini2=c(1000,1000))
plotcurves(fit)
fit <- quickpsy(dat_dl_800_2, FASE, RESP, guess = T, lapses = T, pini =c(50,50,0,0))
plotcurves(fit)
fit <- quickpsy(dat_dl_800_2, FASE, RESP, guess = T, lapses = T,
                pini =c(0,0,-1,-1), pini2 = c(1000, 1000, 1, 1))
plotcurves(fit)
fit <- quickpsy(dat_dl_800_2, FASE, RESP, guess = T, pini =c(50,50,0))
plotcurves(fit)
fit <- quickpsy(dat_dl_800_2, FASE, RESP, guess = T, pini =c(0,0,-1),
                pini2 =c(1000,1000,1))
plotcurves(fit)



plotcurves(fit_cum_normal_fun)
plotthresholds(fit)

