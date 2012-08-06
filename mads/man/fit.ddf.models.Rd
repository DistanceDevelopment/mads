\name{fit.ddf.models}
\alias{fit.ddf.models}
\title{Refits the detection functions to the resampled data}
\usage{
  fit.ddf.models(ddf.dat.working, ddf.models, criterion)
}
\arguments{
  \item{ddf.dat.working}{list of dataframes containing the
  data to which the models will be fitted}

  \item{ddf.models}{list of unique character vectors giving
  the names of the ddf objects for each species.}

  \item{criterion}{character option specifying the model
  selection criteria - "AIC", "AICc" or "BIC".}
}
\value{
  list of ddf objects
}
\description{
  Fits all the models named in ddf.models to the associated
  data supplied in ddf.dat.working. If more than one model
  is supplied for any species the model with the minimum
  selection crieteria will be selected.
}
\note{
  Internal function not intended to be called by user.
}
\author{
  Laura Marshall
}

