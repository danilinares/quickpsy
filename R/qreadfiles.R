#' qreadfiles
#' @export
qreadfiles <- function(path, ...) {
  arguments <- c(as.list(environment()), list(...))
  arguments[1] <- NULL

  namesfun <- function(d) {
    namefile <- paste0(path, paste(unlist(d), collapse = ''), '.txt')
    data.frame(namefile, exist = file.exists(namefile))
  }
  namefiles <- expand.grid(arguments) %>%
    group_by_(.dots = names(arguments)) %>% do(namesfun(.))

  namefiles %>% filter(exist) %>%
    group_by_(.dots = names(arguments)) %>%
    do(read.table(.$namefile, header = T))

}
