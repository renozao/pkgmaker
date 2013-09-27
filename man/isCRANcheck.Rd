\name{isCRANcheck}
\alias{isCHECK}
\alias{isCRANcheck}
\alias{isCRAN_timing}
\title{Package Check Utils}
\usage{
  isCRANcheck(...)

  isCRAN_timing()

  isCHECK()
}
\arguments{
  \item{...}{each argument specifies a set of tests to do
  using an AND operator. The final result tests if any of
  the test set is true. Possible values are: \describe{
  \item{\code{'timing'}}{Check if the environment variable
  \code{_R_CHECK_TIMINGS_} is set, as with the flag
  \code{'--timing'} was set.} \item{\code{'cran'}}{Check if
  the environment variable \code{_R_CHECK_CRAN_INCOMING_}
  is set, as with the flag \code{'--as-cran'} was set.} }}
}
\description{
  \code{isCRANcheck} tries to identify if one is running
  CRAN checks.

  \code{isCRAN_timing} tells if one is running CRAN check
  with flag \code{'--timing'}.

  \code{isCHECK} checks both CRAN expected flags and the
  value of environment variable
  \code{_R_CHECK_RUNNING_UTESTS_}. It will return
  \code{TRUE} if such variable is set to anything not
  equivalent to \code{FALSE}. For example, the function
  \code{\link{utest}} sets it to the name of the package
  being checked
  (\code{_R_CHECK_RUNNING_UTESTS_=<pkgname>}), but unit
  tests run as part of unit tests vignettes are run with
  \code{_R_CHECK_RUNNING_UTESTS_=FALSE}, so that developers
  cann run all tests.
}
\details{
  Currently \code{isCRANcheck} returns \code{TRUE} if the
  check is run with either environment variable
  \code{_R_CHECK_TIMINGS_} (as set by flag
  \code{'--timings'}) or \code{_R_CHECK_CRAN_INCOMINGS_}
  (as set by flag \code{'--as-cran'}).

  Note that the checks performed on CRAN farms are not
  always run with such flags, so there is no guarantee this
  function identifies such runs, and one should rely custom
  environment variables to enable specific tests or
  examples.
}
\examples{
isCHECK()
}
\references{
  Adapted from the function \code{CRAN} in the \pkg{fda}
  package.
}

