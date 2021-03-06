% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create.param.arrays.r
\name{create.param.arrays}
\alias{create.param.arrays}
\title{Creates a list of arrays for storing the ddf results}
\usage{
create.param.arrays(model.names, ddf.models, n, criteria)
}
\arguments{
\item{model.names}{a list of character vectors of model names 
with the elements named by species code}

\item{ddf.models}{a list of all the ddf models named in model.names.}

\item{n}{the number of bootstrap iterations to be completed.}

\item{criteria}{the name of the model selection criteria.}
}
\value{
list of arrays
}
\description{
Creates a list of arrays. These are used to store the parameter estimates,
a record of convergence, selection criteria values and which model was
selected in the case of model uncertainty.
}
\note{
Internal function not intended to be called by user.
}
\author{
Laura Marshall
}
\keyword{utility}
