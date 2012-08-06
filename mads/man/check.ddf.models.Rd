\name{check.ddf.models}
\alias{check.ddf.models}
\title{Checks the list of model names supplied by the user}
\usage{
  check.ddf.models(ddf.models, species.name, dist.names)
}
\arguments{
  \item{ddf.models}{a list of vectors of model names. Each
  list element is named according to the species code.}

  \item{species.name}{a character vector of species codes}

  \item{dist.names}{boolean TRUE if only numbers are
  specified and the Distance naming convention is being
  used. FALSE if complete character model names are
  supplied.}
}
\value{
  updated list of vectors of model names if only numbers
  were supplied otherwise ddf.models is returned unchanged.
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
\seealso{
  \code{execute.multi.analysis}
}
\keyword{data}
\keyword{input}
\keyword{validation}
\keyword{validation,}

