#' one_loglik
#'
#' @export
one_loglik <- function(d, x, k, n, psyfunguesslapses, groups, para) {
  nllfun <- create_nll(d, x, k, n, psyfunguesslapses)
  if (length(groups) == 0) para <- para$para
  else para <- dplyr::semi_join(para, d, by = groups)$para
  data.frame(loglik = -nllfun(para))
}
