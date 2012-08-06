\name{create.warning.storage}
\alias{create.warning.storage}
\title{Checks the list of model names supplied by the user}
\usage{
  create.warning.storage()

  create.warning.storage()
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

  This object will be used to store all warning messages
  generated while the bootstrap is being run. This ensures
  that a summary of all warning messages are available to
  the user on completion. If an object called MAE.warnings
  already exists in the workspace the user is asked to
  remove this object begore running the multi-analysis
  again.
}
\note{
  Internal function not intended to be called by user.

  Internal function not intended to be called by user.
}
\author{
  Laura Marshall

  Laura Marshall
}
\seealso{
  \code{\link{process.warnings}}
}
\keyword{utility}

