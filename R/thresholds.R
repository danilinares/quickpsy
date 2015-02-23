#' thresholds
#'
#' @export
thresholds <- function(qp, prob = NULL, log = F) {
  if (is.null(prob)) stop('You need to specify the value of prob', call. = F)
  qp$para %>% do(one_threshold(., prob, log,
                               qp$funname, qp$guess, qp$lapses, qp$curves))
}



