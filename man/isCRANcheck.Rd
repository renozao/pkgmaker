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
  \item{...}{each argument specifies a set of tests to do using an AND operator.
The final result tests if any of the test set is true.
Possible values are:
\describe{
\item{\code{'timing'}}{Check if the environment variable \code{_R_CHECK_TIMINGS_} is set,
as with the flag \code{'--timing'} was set.}
\item{\code{'cran'}}{Check if the environment variable \code{_R_CHECK_CRAN_INCOMING_} is set,
as with the flag \code{'--as-cran'} was set.}
}}
}
\description{
  \code{isCRANcheck} \strong{tries} to identify if one is
  running CRAN-like checks.

  \code{isCRAN_timing} tells if one is running CRAN check
  with flag \code{'--timing'}.

  Currently, \code{isCHECK} checks both CRAN expected
  flags, the value of environment variable
  \code{_R_CHECK_RUNNING_UTESTS_}, and the value of option
  \code{R_CHECK_RUNNING_EXAMPLES_}. It will return
  \code{TRUE} if any of these environment variables is set
  to anything not equivalent to \code{FALSE}, or if the
  option is \code{TRUE}. For example, the function
  \code{\link{utest}} sets it to the name of the package
  being checked
  (\code{_R_CHECK_RUNNING_UTESTS_=<pkgname>}), but unit
  tests run as part of unit tests vignettes are run with
  \code{_R_CHECK_RUNNING_UTESTS_=FALSE}, so that all tests
  are run and reported when generating them.
}
\details{
  Currently \code{isCRANcheck} returns \code{TRUE} if the
  check is run with either environment variable
  \code{_R_CHECK_TIMINGS_} (as set by flag
  \code{'--timings'}) or \code{_R_CHECK_CRAN_INCOMINGS_}
  (as set by flag \code{'--as-cran'}).

  \strong{Warning:} the checks performed on CRAN check
  machines are on purpose not always run with such flags,
  so that users cannot effectively "trick" the checks. As a
  result, there is no guarantee this function effectively
  identifies such checks. If really needed for honest
  reasons, CRAN recommends users rely on custom dedicated
  environment variables to enable specific tests or
  examples.
}
\examples{
\dontshow{# roxygen generated flag
options(R_CHECK_RUNNING_EXAMPLES_=TRUE)
}

isCHECK()
}
\references{
  Adapted from the function \code{CRAN} in the \pkg{fda}
  package.

  \url{https://github.com/renozao/roxygen2}
}

