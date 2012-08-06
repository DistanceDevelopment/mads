\name{execute.multi.analysis}
\alias{execute.multi.analysis}
\title{Performs Multiple Analyses on Distance Data}
\usage{
  execute.multi.analysis(region.table, sample.table,
    obs.table, bootstrap, bootstrap.options = list(),
    covariate.uncertainty = NULL, ddf.models,
    ddf.model.options = list(),
    species.code.definitions = NULL,
    species.presence = NULL)
}
\arguments{
  \item{region.table}{dataframe of region records -
  Region.Label and Area}

  \item{sample.table}{dataframe of sample records -
  Region.Label, Sample.Label, Effort}

  \item{obs.table}{dataframe of observation records with
  fields object, Region.Label, and Sample.Label which give
  links to sample.table, region.table and the data records
  used in \code{model}}

  \item{bootstrap}{if TRUE resamples data to obtain
  variance estimate}

  \item{bootstrap.options}{a list of options that can be
  set 1) n: number of repetitions 2) resample: how to
  resample data ("samples", "observations")}

  \item{covariate.uncertinaty}{dataframe containing
  information used to parametrically resample data or NULL
  if not required}

  \item{ddf.models}{a list of character/numeric vectors of
  model names/suffixes with the elements named by species
  code}

  \item{ddf.model.options}{a list of options 1) if true
  numeric vectors in ddf.models are suffixes (see details)
  2) selection.criterion either "AIC", "AICc" or "BIC"}

  \item{species.code.definitions}{a list with an element
  for each unidentified code which contains a vector of
  corresponding identified species codes or NULL if not
  required}

  \item{species.presence}{must be specified if
  species.code.definitions is specified. A list with an
  element for each strata which contains the vector of
  species codes present in that strata}
}
\description{
  Analyses are performed for multiple species contained
  within the same dataset. Individual detection function
  analyses of each species must have already been completed
  using the \code{ddf} function in the \code{mrds} library.
  This function may then perform additional tasks such as
  assessing variance via a non-parametric bootstrap,
  including covariate variability via a parametric
  bootstrap, including model uncertainty and dealing with
  species codes which relate to unidentified sightings.
}
\details{
  The fitting code in this function obtains its data from
  the ddf objects already created in the workspace by
  running the \code{ddf} function.

  If you wish to include model uncertainty then each model
  which you wish to be included in the analyses must have
  already been called and exist as a \code{"ds" "ddf"}
  object in the workspace. The \code{ddf.models} argument
  tells this function which \code{"ds" "ddf"} objects in
  the work space are associated with which species code in
  the dataset. This object must be constructed as a list of
  vectors. Each element in the list must be named
  corresponding to one of the species codes in the dataset
  and contain a vector of object names. In the case when
  the models have the same naming convention as the
  software \code{Distance} (e.g. \code{ddf.1},
  \code{ddf.2}, \code{ddf.5}) then these vectors may simply
  be the numeric suffixes and the
  \code{distance.naming.conv} element of the
  \code{ddf.model.options} list should be set to
  \code{TRUE}. Otherwise this list must contain character
  vectors containing the full object names and the
  \code{distance.naming.conv} option should be left as
  FALSE.

  For the majority of analyses the variance will be
  estimated using a non-parametric bootstrap, indicated by
  the \code{bootstrap} argument. You may select options for
  the bootstrap using the \code{bootstrap.options}
  argument. This is a list with elements specifying the
  number of repetitions and whether to resample samples
  within strata (\code{$resample = "samples"}) or
  observations withing strata (\code{$resample =
  "observations"}). In addition, the
  \code{bootstrap.covariates} is a boolean argument
  specifying whether or not a parametric bootstrap should
  be performed on any of the covariates. The details of
  which variables should be resampled and from which
  distributions should be entered in the
  \code{covariate.uncertainty} dataframe. This dataframe
  should contain 7 columns with the following names:
  \code{variable.layer}, \code{variable.name},
  \code{variable.correction.factor},
  \code{uncertainty.layer}, \code{uncertainty.name},
  \code{uncertainty.measure} and
  \code{sampling.distribution}. [Currently variable.layer
  and uncertainty.layer are not implemented]. The
  \code{variable.name} and \code{uncertainty.name} should
  be the names of the variable in the dataset giving the
  covariate to be resampled and the variable containing the
  uncertainty respectively. The \code{uncertainty.name}
  should specify what values the uncertainty variable
  contains and should be one of \code{"sd"}, \code{"var"}
  or \code{"CV"}. The \code{sampling.distribution} should
  specify one of the following distributions to
  parametrically resample from \code{"Normal"},
  \code{"Normal.Absolute"}, \code{"Lognormal.BC"},
  \code{"Poisson"} or \code{"TruncPoissonBC"}. The remaning
  column in this dataset,
  \code{variable.correction.factos}, allows the user to
  specify a value by which the variable should be scaled.
  If this is not reqied this should be set to 1.

  If there are unidentified sightings in the dataset then
  the \code{unidentified.sightings} argument should be
  \code{true} and a \code{species.code.definitions} list
  should be provided. This list must contain one element
  for every unidentified species code which should be named
  according to this code. Each element will contain a
  vector of identified species codes corresponding to those
  species which the unidentified code could have
  potentially been. This function uses this information to
  prorate the abundance estimated from the unidentified
  species codes to the relevant abundances from the
  identified codes. The prorating is done individually for
  each strata. The function can be forced not to prorate to
  any given species in any selected strata using the
  \code{species.presence} argument. This is a list
  containing one element for each strata, each must be
  named using the appropriate strata name. Each element
  should contain a vector of identified species codes
  corresponding to which species are present in each
  strata.
}
\examples{
data(ETP.data)
region<<-ETP.data$region
egdata<<-ETP.data$egdata
samples<<-ETP.data$samples
obs<<-ETP.data$obs
ddf.1=ddf(dsmodel = ~mcds(key = "hn", formula = ~1), data = egdata, method = "ds", meta.data = list(width = 4))
}
\author{
  Laura Marshall
}
\references{
  Marques, F.F.C. and S.T. Buckland. 2004. Covariate models
  for the detection function. In: Advanced Distance
  Sampling, eds. S.T. Buckland, D.R.Anderson, K.P. Burnham,
  J.L. Laake, D.L. Borchers, and L. Thomas.  Oxford
  University Press.  Gerrodette, T. and Forcada, J. 2005
  Non-recovery of two spotted and spinner dolphin
  populations in the eastern tropical Pacific Ocean. Marine
  Ecology Progress Series, 291:1-21.
}
\seealso{
  \code{\link{ddf.ds}},
  \code{\link{ddf.io}},\code{\link{ddf.io.fi}},
  \code{\link{ddf.trial}},\code{\link{ddf.trial.fi}},\code{\link{ddf.rem}},\code{\link{ddf.rem.fi}}
}
\keyword{~distance}
\keyword{covariate}
\keyword{model}
\keyword{sampling,}
\keyword{sightings,}
\keyword{uncertainty}
\keyword{uncertainty,}
\keyword{unidentified}

