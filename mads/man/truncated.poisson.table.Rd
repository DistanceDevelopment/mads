\name{truncated.poisson.table}
\alias{truncated.poisson.table}
\docType{data}
\title{
Look up table for zero-truncated Poisson distribution
}
\description{
Gives the lambda values for the zero-truncated Poisson distribution which
generate values with the specified mean. Used in the implementation of the
bias corrected zero-truncated Poisson distribution.
}
\usage{data(truncated.poisson.table)}
\format{
  A data frame with 20 observations on the following 2 variables.
  \describe{
    \item{\code{mean}}{a numeric vector}
    \item{\code{lambda}}{a numeric vector}
  }
}
\keyword{datasets}
