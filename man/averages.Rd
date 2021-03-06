% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/averages.R
\name{averages}
\alias{averages}
\title{Perform the averages
\code{averages} perform the averages}
\usage{
averages(d, x, x_str, k, n, log, grouping)
}
\arguments{
\item{d}{The input data frame.}

\item{x}{Name of the explanatory variable.}

\item{x_str}{String with the name of the explanatory variable.}

\item{k}{Name of the response variable.}

\item{n}{Only necessary if \code{k} refers to the number of trials
in which a yes-type response was given. It corresponds to the name of the
variable indicating the total number of trials.}

\item{log}{If \code{TRUE}, the logarithm in base 10 of the explanatory
variable is used to fit the curves (default is \code{FALSE}).}

\item{grouping}{Name of the grouping variables. It should be specified as
\code{grouping = c("variable_name1", "variable_name2")}.}
}
\description{
Perform the averages
\code{averages} perform the averages
}
\keyword{internal}
