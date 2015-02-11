### example from Psychophysics: a practical introduction
logx <- c(rep(-2,4),rep(-1,4),rep(0,4),rep(1,4),rep(2,4))
resp <- c(1,0,1,0,0,1,1,1,1,1,0,1,1,1,1,0,1,1,1,1)


dat <- data.frame(logx, resp)
fit <- quickpsy(dat, logx, resp, guess = .5)

glmmod <- glm(cbind(resp,n-resp)~logx, data=fit$averages, family=binomial(probit))

# The likelihoods are different because logLik calculates the full
# likelihood including binomial coefficients. In any case, the absolute
# value of the likelihood doesn't matter

fit$loglik  # this corresponds to the value given in Psychophyics: a practical introduction
logLik(glmmod)

plotcurves(fit)
