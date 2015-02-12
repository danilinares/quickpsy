#' one_parameters
#'
#' @export
one_parameters <- function(d, x, k, n, psyfunguesslapses, funname,
                              pini, guess, lapses, pini2, groups) {

  if (length(groups) == 0) pini <- pini$pini
  else pini <- semi_join(pini, d, by = groups)$pini

  nllfun <- create_nll(d, x, k, n, psyfunguesslapses)

  if (!is.null(pini2)) {
    mod <- DEoptim(nllfun, lower = pini, upper = pini2)$optim
    para <- mod$bestmem
  }
  else para <- optim(pini, nllfun)$par

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

  data.frame(paran = paste0('p', seq(1, length(para))), para)
}


