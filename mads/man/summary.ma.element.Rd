\name{summary.ma.element}
\alias{print.summary.ma}
\alias{summary.ma.element}
\title{Print a summary of an element of a multi-analysis result correcponding to
a single species included in the analyses.}
\usage{
  \method{print}{summary.ma} (x, species = NULL, ...)
}
\arguments{
  \item{x}{a summary of \code{ma} model object}

  \item{\dots}{unspecified and unused arguments for S3
  consistency}
}
\value{
  NULL
}
\description{
  Provides a summary of the fitted detection probability
  model parameters, model selection criterion, and
  optionally abundance in the covered (sampled) region and
  its standard error. What is printed depends on the
  corresponding call to summary.
}
\author{
  Laura Marshall
}
\seealso{
  \code{\link{summary.ma}}
}
\keyword{utility}

