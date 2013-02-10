\name{isCRANcheck}
\alias{isCRANcheck}
\alias{isCRAN_timing}
\title{Package Check Utils}
\usage{
  isCRANcheck(what = "timing")

  isCRAN_timing()
}
\arguments{
  \item{what}{type of CRAN check to test for. Possible
  values are: \describe{ \item{\code{'timing'}}{Check if
  the flag \code{'--timing'} was set.} }}
}
\description{
  \code{isCRANcheck} tells if one is running CRAN check.

  \code{isCRAN_timing} tells if one is running CRAN check
  with flag \code{'--timing'}.
}
\references{
  Adapted from the function \code{CRAN} in the \pkg{fda}
  package.
}

