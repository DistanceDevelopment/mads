\name{accumulate.results}
\alias{accumulate.results}
\title{Enters the prorated results into the bootstrap.results array}
\usage{
  accumulate.results(n, bootstrap.results,
    prorated.results)
}
\arguments{
  \item{n}{index of the current bootstrap iteration}

  \item{bootstrap.results}{list of 4-dimensional arrays
  containing the bootstrap results}

  \item{prorated.results}{list of data objects similar to
  the dht class}
}
\value{
  list of 4-dimensional arrays containing the updated
  bootstrap results
}
\description{
  Enters the prorated results into the bootstrap.results
  array
}
\author{
  Laura Marshall
}

