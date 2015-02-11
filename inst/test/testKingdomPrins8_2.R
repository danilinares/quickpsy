logcon <- -2:2
noada <- data.frame(logcon, k = c(61, 70, 81, 92, 97), cond = 'No adapt')
ada <- data.frame(logcon, k = c(59, 59, 67, 86, 91), cond = 'Adapt')
data <- rbind(noada, ada) %>% mutate(n = 100)

datacombined <- data %>% group_by(logcon) %>%
  summarise(k = sum(k), n = sum(n)) %>% mutate(cond = 'Combined')

data <- rbind(data, datacombined)

fit <- quickpsy(data, logcon, k, n, within = .(cond),
                guess = .5, fun = logistic_fun)
plotcurves(fit)

# likelihood of the lesser model
lik_lesser <- exp((fit$loglik %>% filter(cond == 'Combined'))$loglik)

# likelihood of the fuller model
lik_fuller <-exp((fit$loglik %>% filter(cond == 'No adapt'))$loglik +
      (fit$loglik %>% filter(cond == 'Adapt'))$loglik)

lik_ratio <- lik_lesser / lik_fuller


bootstrap <- function(qp, pini = NULL, pini2 = NULL) {
  calculate_boot <- function(d, x, k, n, psyfunguesslapses, funname,
                             guess, lapses, pini, pini2) {

    if (length(qp$groups) == 0) ypred <- qp$ypred
    else ypred <- semi_join(qp$ypred, d)

    calculate_para <- function(f)
      parameters(f, x, k, n, psyfunguesslapses, funname,
                 pini, guess, lapses, pini2)$para

    create_fake_data <- function(f, mle){
      kfake <- rbinom(length(f[[x]]), f[[n]], mle)
      f[[k]] <- kfake
      f$y <- kfake / f[[n]]
      f
    }

    b <- boot(d, calculate_para, R = 3, sim='parametric',
                    ran.gen = create_fake_data, mle = ypred$ypred)
    fake_par <- b$t
    colnames(fake_par) <- paste0('p',1:length(fake_par[1,]))
    long <- data.frame(fake_par, sample = 1:length(fake_par[,1]))
    long %>% gather(paran, para, -sample) %>% arrange(sample)

  }
  qp$averages %>% do(calculate_boot(., qp$x, qp$k, qp$n, qp$psyfunguesslapses,
                                    qp$funname, qp$guess, qp$lapses,
                                    pini, pini2))
}
zz <- bootstrap(fit)



