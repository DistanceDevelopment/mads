\name{check.species.presence}
\alias{check.species.presence}
\title{Checks the list of species presence definitions supplied by the user}
\usage{
  check.species.presence(species.presence, species.name,
    strata.names)
}
\arguments{
  \item{species.presence}{a list with an element for each
  strata which contains a vector of corresponding species
  codes for those species which occur in that strata or
  NULL if it is to be data driven.}

  \item{species.name}{a vector of species names for which
  model names were supplied in the ddf.models list passed
  to execute.multi.analysis by the user.}
}
\value{
  updated species.presence list
}
\description{
  Performs a numer of checks to ensure the list has been
  defined correctly. If no list was specified one is
  created assuming all species are present in all strata.
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
\keyword{input}
\keyword{validation}

