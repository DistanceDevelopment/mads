\name{process.bootstrap.results}
\alias{process.bootstrap.results}
\title{Summarises the bootstrap results.}
\usage{
  process.bootstrap.results(bootstrap.results)
}
\arguments{
  \item{bootstrap.results}{list of arrays containg results
  from the repeated analyses.}
}
\value{
  ma object a list of summary statistics for each species
}
\description{
  Creates summary statistics for each species. These
  consist of dataframes relating to summaries, abundance
  (N) and density (D) for both individuals and clusters. In
  addition, summary statistics for expected cluster size
  (Expected.S) are also calculated.
}
\note{
  Internal functions not intended to be called by user.
}
\author{
  Laura Marshall
}

