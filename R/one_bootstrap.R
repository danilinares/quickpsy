#' one_bootstrap
#'
#' @export
one_bootstrap <- function(d, x, k, n, psyfunguesslapses, funname,
                           guess, lapses, pini, pini2, B, groups, ypred) {

  if (length(groups) != 0) ypred <- semi_join(ypred, d, by = groups)

  calculate_para <- function(f)
    parameters(f, x, k, n, psyfunguesslapses, funname,
               pini, guess, lapses, pini2, groups)$para

  create_fake_data <- function(f, mle){
    kfake <- rbinom(length(f[[x]]), f[[n]], mle)
    f[[k]] <- kfake
    f$y <- kfake / f[[n]]
    f
  }

  b <- boot(d, calculate_para, R = B, sim='parametric',
            ran.gen = create_fake_data, mle = ypred$ypred)
  fake_par <- b$t
  colnames(fake_par) <- paste0('p',1:length(fake_par[1,]))
  long <- data.frame(fake_par, sample = 1:length(fake_par[,1]))
  long %>% gather(paran, para, -sample) %>% arrange(sample)
}

