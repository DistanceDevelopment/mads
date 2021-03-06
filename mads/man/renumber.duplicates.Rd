% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/renumber.duplicates.R
\name{renumber.duplicates}
\alias{renumber.duplicates}
\title{Renumbers the object IDs for the duplicate observations generated when
bootstrapping}
\usage{
renumber.duplicates(ddf.dat, obs.table, double.observer)
}
\arguments{
\item{ddf.dat}{dataframe containing a single dataset with duplicate 
observations}

\item{obs.table}{dataframe of observation records with fields object,
Region.Label, and Sample.Label which give links to sample.table,
region.table and the data records used in \code{model}}

\item{double.observer}{boolean indicating if it is a double observer survey}
}
\value{
list with 2 elements:
  ddf.dat dataframe containing a single dataset with new and unique 
    observation IDs 
  obs.table the updated obs.table dataframe containing the new
    observation IDs
}
\description{
Find the largest object ID and renumbers all duplicate IDs starting form 
this value. The information for the duplicates is also added to the obs.table
}
\note{
Internal function not intended to be called by user.
}
\seealso{
\code{resample.data}
}
\author{
Laura Marshall
}
\keyword{internal}
