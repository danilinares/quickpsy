library(dplyr)
library(ggplot2)
library(MPDiR)
library(quickpsy)

### Hecht, Shlaer and Pirenne
prob <- .6
data(HSP)
names(HSP) <- c('Quanta', 'PerCent', 'N', 'Obs', 'Run')
HSP <- HSP %>% mutate(NumYes = round(N * PerCent / 100), NumNo = N- NumYes,
                      logQ = log(Quanta), y = PerCent / 100)

## one fit
SHR1 <- HSP %>% filter(Obs == 'SH', Run == 'R1')

SHR1glm <- glm(cbind(NumYes, NumNo) ~ logQ, data = SHR1,
                family = binomial(probit))
xseq <- seq(3.5, 6.5, length = 100)
yseq <- predict(SHR1glm, newdata = data.frame(logQ = xseq), type = 'response')
SHR1curve <- data.frame(x = xseq, y = yseq)

m <- -coef(SHR1glm)[[1]] / coef(SHR1glm)[[2]]
sta <- 1 / coef(SHR1glm)[[2]]
thre <- qnorm(prob, m, sta)

layer_curve <- geom_line(data = SHR1curve, aes(x = x, y = y), color = 'red')
layer_thre <- geom_linerange(data = data.frame(thre),
                             aes(x = thre, ymin = 0, ymax= prob), color = 'red')

pSHR1 <- ggplot(SHR1) +
  geom_point(aes(x = logQ, y = y)) +
  layer_curve + layer_thre
pSHR1

## one fit quickpsy
fit <- quickpsy(SHR1, logQ, NumYes, N, prob = prob)
plotcurves(fit)
plotcurves(fit) + layer_curve + layer_thre


## several fits quickpsy
fit <- quickpsy(HSP, logQ, NumYes, N, prob = prob, within = .(Run), random = .(Obs), B = 10)
plotcurves(fit, xpanel = Obs, ypanel = Run)



