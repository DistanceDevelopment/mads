\name{rtpois}
\alias{rtpois}
\title{Checks the list of model names supplied by the user}
\usage{
  rtpois(N, mean = NA, lambda = NA)
}
\arguments{
  \item{ddf.models}{}

  \item{species.name}{}

  \item{dist.names}{}
}
\description{
  Performs various checks on the model names supplied by
  the user. If the multi-analysis engine is being called
  from Distance then the model names are transformed from
  numbers to names. This also checks that the data in each
  model are the same across species codes to ensure valid
  comparison using the AIC/AICc/BIC selection criteria.
}
\note{
  Internal function not intended to be called by user.
}
\author{
  Laura Marshall
}

