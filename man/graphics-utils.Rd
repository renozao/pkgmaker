\name{graphics-utils}
\alias{graphics-utils}
\alias{mfrow}
\title{Utility Functions for Graphics}
\usage{
  mfrow(n)
}
\arguments{
  \item{n}{number of plots to be arranged.}
}
\description{
  Utility Functions for Graphics

  \code{mfrow} returns a 2-long numeric vector suitable to
  use in \code{\link{par}(mfrow=x)}, that will arrange
  \code{n} panels in a single plot.
}
\examples{
\dontshow{# roxygen generated flag
options(R_CHECK_RUNNING_EXAMPLES_=TRUE)
}

mfrow(1)
mfrow(2)
mfrow(3)
mfrow(4)
mfrow(10)
}

