#' \code{parini} parini
#' @keywords internal
parini <- function(averages, parini, psych_fun, grouping_without_fun) {

  if (length(grouping_without_fun) == 0) conditions <- c()
  else {
    conditions <- averages %>%
      ungroup() %>%
      distinct_at(grouping_without_fun)
    }

  if (is.atomic(parini)) {
    parini <- conditions %>%
      crossing(data.frame(parn = paste0("p", seq(1, length(parini))),
                      par = parini))
  }

  else if (is.list(parini) & !is.data.frame(parini)) {
    parini <- matrix(unlist(parini), ncol = 2, byrow = TRUE)

    parini <- conditions %>%
      crossing(data.frame(parn = paste0("p", seq(1, length(parini[,1]))),
                      parmin = parini[,1],
                      parmax = parini[,2]))
  }

  else if (is.data.frame(parini)) {
    parini <- parini
  }

  parini %>% group_by(!!!syms(grouping_without_fun))
}
