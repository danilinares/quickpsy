% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cum_normal_fun.R
\name{cum_normal_fun}
\alias{cum_normal_fun}
\title{Cumulative normal function}
\usage{
cum_normal_fun(x, p)
}
\arguments{
\item{x}{Vector of values of the explanatory variable.}

\item{p}{Vector of parameters \code{p = c(mean, standard_deviation)}.}
}
\value{
Probability at each \code{x}.
}
\description{
Cumulative normal function.
}
\examples{
library(ggplot2)
xseq <- seq(0,4,.01)
yseq <- cum_normal_fun(xseq, c(2, .5))
curve <- data.frame(x = xseq, y = yseq)
ggplot2::ggplot(curve, aes(x = x, y = y)) + geom_line()
}
\seealso{
\code{\link{inv_cum_normal_fun}}
}
