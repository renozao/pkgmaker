\name{extractLocalFun}
\alias{allFormals}
\alias{extractLocalFun}
\title{Extracting Local Function Definition}
\usage{
  extractLocalFun(f)

  allFormals(f)
}
\arguments{
  \item{f}{definition of the wrapper function}
}
\value{
  a function

a paired list like the one returned by \code{\link{formals}}.
}
\description{
  \code{extractLocalFun} Extracts local function from
  wrapper functions of the following type, typically used
  in S4 methods: \samp{ function(a, b, ...)\{ .local <-
  function(a, b, c, d, ...)\{\} .local(a, b, ...) \} }

  Works for methods that are created (setMethod) as a
  wrapper function to an internal function named .local.
}

