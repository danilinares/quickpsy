#' @keywords internal
thresholds_dif <- function(thresholds) {

  thre <- thresholds %>%
    dplyr::select(-prob)

  combinations <- as.data.frame(t(combn(nrow(thre), 2)))

  select_rows <- function(V1, V2) {
    cond1 <- thre[V1,]
    cond2 <- thre[V2,]
    names(cond2) <- paste0(names(cond2), "2")
    bind_cols(cond1, cond2)
  }

  combinations %>%
    rowwise() %>%
    mutate(temp = list(select_rows(V1, V2))) %>%
    unnest(temp) %>%
    mutate(dif = thre - thre2) %>%
    dplyr::select(-V1, -V2)

}
