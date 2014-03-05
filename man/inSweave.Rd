\name{inSweave}
\alias{inSweave}
\title{Identifying Sweave Run}
\usage{
  inSweave()
}
\value{
  \code{TRUE} or \code{FALSE}
}
\description{
  Tells if the current code is being executed within a
  Sweave document.
}
\examples{
\dontshow{# roxygen generated flag
options(R_CHECK_RUNNING_EXAMPLES_=TRUE)
}

# Not in a Sweave document
inSweave()

# Within a Sweave document
}

