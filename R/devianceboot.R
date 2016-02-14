#' Calculates the bootsrap deviances
#'
#' \code{deviance} calculates the bootstrap deviances.
#' @param qp output from quickpsy
#' @export
#' @examples
#' library(MPDiR) # contains the Vernier data
#' fit <- quickpsy(Vernier, Phaseshift, NumUpward, N,
#'                 grouping = .(Direction, WaveForm, TempFreq), B = 20)
#' devianceboot(fit)
#' @export
devianceboot <- function(qp) {
  if (length(qp$groups) == 0)
    logliksboot <- qp$logliksboot %>% group_by_('sample')
  else
    logliksboot <- qp$logliksboot %>%
      group_by_(.dots = c(qp$groups, 'sample'))

  allgroups <- as.character(groups(qp$logliksboot))

  logliksboot %>% do(one_deviance(., allgroups, qp$logliksbootsaturated))
}


