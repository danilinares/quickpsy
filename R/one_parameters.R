#' one_parameters
#'
#' @export
one_parameters <- function(d, x, k, n, psyfunguesslapses, funname,
                              pini, piniset, guess, lapses, DE, groups) {

  nllfun <- create_nll(d, x, k, n, psyfunguesslapses)

  if (DE) {
    if (is.list(pini)) {
      pini <- matrix(unlist(pini), ncol = 2, byrow = T)
      mod <- DEoptim::DEoptim(nllfun, lower = pini[,1], upper = pini[,2])$optim
      para <- mod$bestmem
    }
    else stop('pini should be a list of the type list(c(p1min, p1max), c(p2min, p2max),...')
  }
  else {

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
  data.frame(paran = paste0('p', seq(1, length(para))), para)
}

#
#     nllfun <- create_nll(d, x, k, n, psyfunguesslapses)
#
#     if (!is.null(pini2)) {
#       mod <- DEoptim(nllfun, lower = pini, upper = pini2)$optim
#       para <- mod$bestmem
#     }
#     else para <- optim(pini, nllfun)$par
#   if (funname %in% names(get_functions())) {
#     if (is.logical(guess) && !is.logical(lapses))
#       if (para[3]<0) message('Warning: negative guess rate')
#     if (!is.logical(guess) && is.logical(lapses))
#     if (para[3]<0) message('Warning: negative guess rate')
#     if (is.logical(guess) && is.logical(lapses)) {
#         if (para[3]<0) message('Warning: negative guess rate')
#         if (para[4]<0) message('Warning: negative lapses rate')
#     }
#   }


