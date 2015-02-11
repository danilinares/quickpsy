#' thresholds
#'
#' @export
thresholds <- function(qp, prob = NULL, log = F) {
print('thresholds')
  qp$para %>% do(one_threshold(., prob, log,
                               qp$funname, qp$guess, qp$lapses))
}



