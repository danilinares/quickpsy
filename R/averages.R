#' Perform the averages
#' \code{averages} perform the averages
#' @keywords internal
averages <- function(d, x, x_str, k, n, log, grouping) {

  grouping_x <- c(grouping, x_str)

  averages <- d %>%
    group_by(!!!syms(grouping_x))


  if (is.null(n)) {
    averages <- averages %>%
      summarise(n = n(), k = sum(!!k), .groups = "drop_last")
  }
  else {
    averages <- averages %>%
      summarise(n = sum(!!n), k = sum(!!k), .groups = "drop_last")
  }

  averages <- averages %>% mutate(prob = k / n)

  if (log) {
    averages <- averages %>%
      mutate(!!quo_name(x) := log(!!x))
    }

  averages
}


