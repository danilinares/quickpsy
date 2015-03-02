#' @keywords internal
#' @export
one_parameters <- function(d, x, k, n, psyfunguesslapses, funname, pini,
                           piniset, guess, lapses, optimization, groups) {
  nllfun <- create_nll(d, x, k, n, psyfunguesslapses)

  if (optimization == 'DE') {
    if (is.data.frame(pini) || is.atomic(pini))
      stop('pini should be specified as a list of the type list(c(para1min, para1max), c(para2min, para2max),...', call. = F)
    else if (is.list(pini)) {
      pini <- matrix(unlist(pini), ncol = 2, byrow = T)
      mod <- DEoptim::DEoptim(nllfun, lower = pini[,1], upper = pini[,2])$optim
      para <- mod$bestmem
    }
    else
      stop('pini should be specified as a list of the type list(c(para1min, para1max), c(para2min, para2max),...', call. = F)

  }
  if (optimization== 'optim') {

    if (piniset) {
      if (is.atomic(pini))
        para <- optim(pini, nllfun)$par
      if (is.list(pini)){
        pini <- matrix(unlist(pini), ncol = 2, byrow = T)
        para <- optim(.5 * (pini[,1] + pini[,2]),
                      nllfun, method = 'L-BFGS-B',
                      lower = pini[,1],
                      upper = pini[,2])$par
      }
    }
    else {
      if (length(groups) == 0) pini <- pini$para
      else pini <- semi_join(pini, d, by = groups)$para

      if (funname == 'weibull_fun') {
        if (pini[1] < 0) pini[1] <- .Machine$double.eps
        if (pini[2] < 0) pini[2] <- .Machine$double.eps
      }
      if (funname == 'cum_normal_fun') {
        if (pini[2] < 0) pini[2] <- 0
      }

      para <- optim(pini, nllfun)$par
    }
  }
  if (optimization != 'DE' && optimization != 'optim')
    stop('optimization should be \'optim \' or \'DE\'.', call. = F)
  data.frame(paran = paste0('p', seq(1, length(para))), para)
}


