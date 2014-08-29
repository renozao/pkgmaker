\name{hasEnvar}
\alias{hasEnvar}
\title{Check Environment Variables}
\usage{
  hasEnvar(x)
}
\arguments{
  \item{x}{environment variable name, as a character vector.}
}
\description{
  Tells if some environment variable(s) are defined.
}
\examples{
\dontshow{# roxygen generated flag
options(R_CHECK_RUNNING_EXAMPLES_=TRUE)
}

hasEnvar('_R_CHECK_TIMINGS_')
hasEnvar('ABCD')
}

