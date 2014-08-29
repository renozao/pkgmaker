\name{hasArg2}
\alias{hasArg2}
\title{Checking for Missing Arguments}
\usage{
  hasArg2(name)
}
\arguments{
  \item{name}{the name of an argument as a character string.}
}
\description{
  This function is identical to \code{\link{hasArg}},
  except that it accepts the argument name as a character
  string. This avoids to have a check NOTE about invisible
  binding variable.
}
\examples{
\dontshow{# roxygen generated flag
options(R_CHECK_RUNNING_EXAMPLES_=TRUE)
}

f <- function(...){ hasArg2('abc') }
f(a=1)
f(abc=1)
f(b=1)
}

