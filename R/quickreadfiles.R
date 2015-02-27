#' Reads several files
#'
#' \code{quickreadfiles} builts a data frame from several txt files. It
#' assumes that in each file, the firt row has the names of the variables.
#' @param path Path of the file (default is the working directory).
#' @param ... arguments of the form name_var = c('value1', 'value2',..)
#' @examples
#' #quickreadfiles(participant = c('aa', 'bb'), experiment = c('exp1','exp2'))
#' #for example, will build a data frame from the files aaexp1.txt, aaexp2.txt,
#' #bbexp1.txt and bbexp2.txt including the names of the variables in the files
#' #and the new two variables names participant and experiment.
#'
#' @export
#'
quickreadfiles <- function(path = getwd(), ...) {
  arguments <- c(as.list(environment()), list(...))
  arguments[1] <- NULL

  namesfun <- function(d) {
    namefile <- paste0(path, paste(unlist(d), collapse = ''), '.txt')
    data.frame(namefile, exist = file.exists(namefile))
  }

  namefiles <- expand.grid(arguments) %>%
    dplyr::group_by_(.dots = names(arguments)) %>%
    dplyr::do(namesfun(.))

  namefiles %>% dplyr::filter(exist) %>%
    dplyr::group_by_(.dots = names(arguments)) %>%
    dplyr::do(read.table(.$namefile, header = T))

}
