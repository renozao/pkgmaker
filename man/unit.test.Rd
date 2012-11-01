\name{unit.test}
\alias{unit.test}
\title{Embedded Unit Tests}
\usage{
  unit.test(x, expr, framework = NULL,
    envir = parent.frame())
}
\arguments{
  \item{x}{single character string used as test
  identifier/label}

  \item{expr}{expression containing the actual test
  commands. It is not evaluated, but only stored in the
  package namespace.}

  \item{framework}{Unit test framework}

  \item{envir}{the definition environment of object
  \code{x}.}
}
\value{
  a test function with no arguments that wrapping around
  \code{expr}
}
\description{
  The function \code{unit.test} provides a way to write
  unit tests embedded within package source files. These
  tests are stored and organised in the package namespace,
  and can be run using the unified interface provided by
  the function \code{link{utest}}. Both Runit and testthat
  tests are supported -- and automatically detected.
}

