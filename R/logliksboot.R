#' Calculates the bootstrap loglikelihoods
#'
#' \code{logliksboot} calculates the bootstraploglikelihoods.
#' @param qp output from quickpsy
#' @export
#' @examples
#' library(MPDiR) # contains the Vernier data
#' fit <- quickpsy(Vernier, Phaseshift, NumUpward, N,
#'                 grouping = .(Direction, WaveForm, TempFreq), B = 20)
#' logliksboot(fit)
#' @export
logliksboot <- function(qp) {
  if (length(qp$groups) == 0)
    avbootstrap <- qp$avbootstrap %>% group_by_('sample')
  else
    avbootstrap <- qp$avbootstrap %>%
      group_by_(.dots = c(qp$groups, 'sample'))

  allgroups <- as.character(groups(qp$avbootstrap))

  avbootstrap %>%
    do(one_loglik(., qp$x, qp$k, qp$n, qp$psyfunguesslapses, allgroups,
                  qp$parbootstrap))
}




