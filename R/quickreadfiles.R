#' Reads several files
#'
#' \code{quickreadfiles} builts a data frame from several txt files. It
#' assumes that in each file, the first row has the names of the variables.
#' @param path Path of the file (default is the working directory).
#' @param extension Specify whether the file extension is 'txt' or 'csv'.
#' @param ... arguments of the form name_var = c('value1', 'value2',..).
#' A new column with variable name name_var is addes to the data frame.
#' @examples
#' # download the 3 files in
#' # https://github.com/danilinares/quickpsy/tree/master/inst/extdata/example1
#' # and add them to your working directory
#' # dat <- quickreadfiles(subject = c('aa', 'bb', 'cc'), session = c('1', '2'))
#' # fit <- quickpsy(dat, phase, resp, grouping=.(subject), lapses = T, guess = T)
#' # plotcurves(fit)
#' @importFrom utils read.csv
#' @import dplyr
#' @export

quickreadfiles <- function(path = getwd(), extension = 'txt', ...) {

  arguments <- c(as.list(environment()), list(...))
  arguments[1] <- NULL
  arguments[1] <- NULL
  exist <- NULL # Joan added (1-4-2015)

  if (extension == 'txt') {
    extensiondot <- '.txt'
    funread <- read.table
  }
  else if (extension == 'csv') {
    extensiondot <- '.csv'
    funread <- read.csv
  }
  else stop('The extension of the files should be txt of csv')


  namefiles <- expand.grid(arguments) %>%
    group_by(!!!syms(names(arguments))) %>%
    unite(namefile, sep = "", remove = FALSE) %>%
    mutate(namefile = paste0(path,'/', namefile, extensiondot)) %>%
    mutate(exist = file.exists(namefile))


  namefiles %>%
    filter(exist) %>%
    mutate(file = list(funread(file = namefile, header = TRUE))) %>%
    dplyr::select(-namefile, -exist) %>%
    unnest(file)

}
