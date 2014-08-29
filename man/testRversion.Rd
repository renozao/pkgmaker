\name{testRversion}
\alias{testRversion}
\title{Testing R Version}
\usage{
  testRversion(x, test = 1L)
}
\arguments{
  \item{x}{target version to compare with.}

  \item{test}{numeric value that indicates the comparison to be carried out.
The comparison is based on the result from
\code{utils::compareVersion(R.version, x)}:
\itemize{
\item 1: is R.version > \code{x}?
\item 0: is R.version = \code{x}?
\item -1: is R.version < \code{x}?
}}
}
\value{
  a logical
}
\description{
  Compares current R version with a given target version,
  which may be useful for implementing version dependent
  code.
}
\examples{
\dontshow{# roxygen generated flag
options(R_CHECK_RUNNING_EXAMPLES_=TRUE)
}

testRversion("2.14")
testRversion("2.15")
testRversion("10")
testRversion("10", test = -1)
testRversion("< 10")
testRversion(Rversion())
testRversion(paste0('=', Rversion()))
}

