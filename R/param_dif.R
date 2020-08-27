#' @keywords internal
#' @importFrom utils combn
#' @importFrom rlang .data
param_dif <- function(param) {

  combinations <- as.data.frame(t(combn(nrow(param %>% filter(.data$parn == "p1")), 2)))
  create_df <- function(df) {
    V1 <- NULL
    V2 <- NULL
    select_rows <- function(V1, V2) {
      cond1 <- df[V1,]
      cond2 <- df[V2,]
      names(cond2) <- paste0(names(cond2), "2")
      bind_cols(cond1, cond2)
    }

    combinations %>%
      rowwise() %>%
      mutate(temp = list(select_rows(.data$V1, .data$V2))) %>%
      unnest(.data$temp) %>%
      mutate(dif = .data$par - .data$par2) %>%
      dplyr::select(-V1, -V2)
  }

  param %>%
    ungroup() %>%
    nest_by(.data$parn)  %>%
    summarise(create_df(.data$data), .groups = "keep")


}
