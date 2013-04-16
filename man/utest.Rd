\docType{methods}
\name{utest}
\alias{utest}
\alias{utest,character-method}
\alias{utest,function-method}
\alias{utest-methods}
\alias{utest,RUnitTestSuite-method}
\title{Running Unit Tests}
\usage{
  utest(x, ...)

  \S4method{utest}{function}(x, run = TRUE)

  \S4method{utest}{character}(x,
    filter = "^runit.+\\\\.[rR]$", fun = "^test\\\\.", ...,
    testdir = "tests", framework = c("RUnit", "testthat"),
    quiet = Sys.getenv("RCMDCHECK") != "FALSE",
    lib.loc = NULL)

  \S4method{utest}{RUnitTestSuite}(x, ..., quiet = FALSE,
    outdir = NULL)
}
\arguments{
  \item{x}{object to which a unit test is attached}

  \item{...}{extra arguments to allow extensions and are
  passed to the unit framework running funcitons.}

  \item{run}{a logical that indicates if the unit test
  should be run}

  \item{filter}{pattern to match files that contain the
  definition of the unit tests functions to run.}

  \item{fun}{patter to match the test functions to run.}

  \item{testdir}{directory where to look for the test
  files}

  \item{framework}{unit test framework}

  \item{quiet}{a logical that indicates if the tests should
  be run silently}

  \item{lib.loc}{path to a library where installed packages
  are searched for. Used is of the form
  \code{x='package:*'}.}

  \item{outdir}{output directory}
}
\description{
  Run unit tests in a variety of settings. This is still
  \strong{very} experimental.
}
\section{Methods}{
  \describe{

  \item{utest}{\code{signature(x = "function")}: Run the
  unit test assoicated to a function. }

  \item{utest}{\code{signature(x = "character")}: Run a
  package test suite }

  \item{utest}{\code{signature(x = "RUnitTestSuite")}: Runs
  a RUnit test suite }

  }
}
\keyword{methods}

