#' @keywords internal
#' @importFrom rlang .data
thresholds_dif <- function(thresholds) {

  V1 <- NULL
  V2 <- NULL

  thre <- thresholds %>%
    select(-.data$prob)

  combinations <- as.data.frame(t(combn(nrow(thre), 2)))

  select_rows <- function(V1, V2) {

    cond1 <- thre[V1,]
    cond2 <- thre[V2,]
    names(cond2) <- paste0(names(cond2), "2")
    bind_cols(cond1, cond2)
  }

  combinations %>%
    rowwise() %>%
    mutate(temp = list(select_rows(.data$V1, .data$V2))) %>%
    unnest(.data$temp) %>%
    mutate(dif = .data$thre - .data$thre2) %>%
    dplyr::select(-V1, -V2)

}
