\name{rtpois}
\alias{rtpois}
\title{Ramdonly generates values from a zero-truncated Poisson distribution}
\usage{
  rtpois(N, mean)

  rtpois(N, mean)
}
\arguments{
  \item{N}{number of values to randomly generate}

  \item{mean}{mean of the generated values}

  \item{N}{number of values to randomly generate}

  \item{mean}{mean of the generated values}
}
\description{
  Generates values from a zero-truncated Poisson
  distribution with mean equal to that specified. It uses a
  look up table to check which value of lambda will give
  values with the requested mean.

  Generates values from a zero-truncated Poisson
  distribution with mean equal to that specified. It uses a
  look up table to check which value of lambda will give
  values with the requested mean.
}
\note{
  Internal function not intended to be called by user.

  Internal function not intended to be called by user.
}
\author{
  Laura Marshall

  Laura Marshall

  data(truncated.poisson.table)
}

