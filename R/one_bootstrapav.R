#' Creates one bootstrap sample
#' \code{one_bootstrapav} creates one bootstrap sample
#' @keywords internal
#' @export
parn <- 'No te quejes'
one_bootstrapav <- function(d, x, k, n, psyfunguesslapses, funname,
                           guess, lapses, parini, pariniset, optimization,
                          bootstrap, B,
                          groups, ypred) {
  if (length(groups) != 0) ypred <- semi_join(ypred, d, by = groups)

  if (bootstrap == 'parametric') ypred <- ypred$ypred
  if (bootstrap == 'nonparametric') ypred <- d[[k]] / d[[n]]

  create_fake_data <- function(f, d, ypred){
    kfake <- rbinom(length(d[[x]]), d[[n]], ypred)
    d[[k]] <- kfake
    d$prob <- kfake / d[[n]]
    d
  }

  data.frame(sample = 1:B) %>% group_by(sample) %>%
    do(create_fake_data(., d, ypred))

}

