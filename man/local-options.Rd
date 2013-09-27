\name{mkoptions}
\alias{mkoptions}
\alias{.options}
\title{Quick Option-like Feature}
\usage{
  mkoptions(...)

  .options(..., .DATA)
}
\arguments{
  \item{...}{list of keys or key-value pairs. For
  \code{mkoptions} these define inital/default key-value
  pairs.}

  \item{.DATA}{a list or an environment with an element
  \code{.options}.}
}
\description{
  \code{mkoptions} is a function that returns a function
  that behaves like \code{\link[base]{options}}, with an
  attached internal/local list of key-value pairs.

  \code{.options} is a low-level function that mimics the
  behaviour of the base function
  \code{\link[base]{options}}, given a set of key-value
  pairs. It is the workhorse function used in
  \code{mkoptions} and package-specific option sets (see
  \code{\link{setupPackageOptions}})
}
\examples{
f <- mkoptions(a=3, b=list(1,2,3))
str(f())
f('a')
f('b')
str(old <- f(a = 10))
str(f())
f(old)
str(f())
}
\seealso{
  \code{\link{setupPackageOptions}}
}

