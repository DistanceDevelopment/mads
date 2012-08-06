\name{resample.covariates}
\alias{resample.covariates}
\title{Parametrcally resamples selected variables in the dataset.}
\usage{
  resample.covariates(ddf.dat.working,
    covariate.uncertainty)

  resample.covariates(ddf.dat.working,
    covariate.uncertainty)
}
\arguments{
  \item{ddf.dat.working}{a list of datasets to be used in
  the analyses}

  \item{covariate.uncertainty}{a dataframe detailing the
  variables to be resampled - variable.layer,
  variable.name, correction.factor.layer, #THIS FUNCTION
  NEEDS TO BE MODIFIED TO TAKE IN CORRECTION FACTOR LAYER
  TOO!  variable.correction.factor , uncertainty.layer,
  uncertainty.name, uncertainty.measure,
  sampling.distribution.}

  \item{ddf.dat.working}{a list of datasets to be used in
  the analyses}

  \item{covariate.uncertainty}{a dataframe detailing the
  variables to be resampled - variable.layer,
  variable.name, correction.factor.layer, #THIS FUNCTION
  NEEDS TO BE MODIFIED TO TAKE IN CORRECTION FACTOR LAYER
  TOO!  variable.correction.factor , uncertainty.layer,
  uncertainty.name, uncertainty.measure,
  sampling.distribution.}
}
\value{
  list of dataframes containing the parametrically
  resampled data

  list of dataframes containing the parametrically
  resampled data
}
\description{
  Parametricelly resamples specified variables in the
  dataset based on a measure of uncertainty and a sampling
  distribution supplied by the user.

  Parametricelly resamples specified variables in the
  dataset based on a measure of uncertainty and a sampling
  distribution supplied by the user.
}
\note{
  Internal function not intended to be called by user.

  Internal function not intended to be called by user.
}
\author{
  Laura Marshall

  Laura Marshall
}
\keyword{data}
\keyword{manipulation}

