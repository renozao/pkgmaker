\name{isCRANcheck}
\alias{isCRANcheck}
\alias{isCRAN_timing}
\title{Package Check Utils}
\usage{
  isCRANcheck(...)

  isCRAN_timing()
}
\arguments{
  \item{...}{each argument specifies a set of tests to do
  using an AND operator. The final result tests if any of
  the test set is true. Possible values are: \describe{
  \item{\code{'timing'}}{Check if the environment variable
  '_R_CHECK_TIMINGS_' is set, as with the flag
  \code{'--timing'} was set.} \item{\code{'cran'}}{Check if
  the environment variable '_R_CHECK_CRAN_INCOMING_' is
  set, as with the flag \code{'--as-cran'} was set.} }}
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

