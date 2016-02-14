#' Calculates the bootstrap loglikelihoods for the saturated model
#'
#' \code{logliks} calculates the bootstrap loglikelihoods for the saturated model.
#' @param qp output from quickpsy
#' @export
#' @examples
#' library(MPDiR) # contains the Vernier data
#' fit <- quickpsy(Vernier, Phaseshift, NumUpward, N,
#'                 grouping = .(Direction, WaveForm, TempFreq), B = 20)
#' logliksbootsaturated(fit)
#' @export
logliksbootsaturated <- function(qp) {
  if (length(qp$groups) == 0)
    avbootstrap <- qp$avbootstrap %>% group_by_('sample')
  else
    avbootstrap <- qp$avbootstrap %>%
      group_by_(.dots = c(qp$groups, 'sample'))

  allgroups <- as.character(groups(qp$avbootstrap))

  avbootstrap %>%
    do(one_logliksaturated(., qp$x, qp$k, qp$n, qp$psyfunguesslapses, allgroups,
                         qp$parbootstrap))
}




