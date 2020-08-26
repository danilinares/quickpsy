#' Creates sequences of x's
x_seq <- function(limits, x, grouping) {

  limits %>%
    summarise(!!quo_name(x) := seq(xmin, xmax, length = 500), .groups = "drop_last") %>%
    group_by(!!!syms(grouping))

}

