\name{utestPath}
\alias{utestPath}
\title{Unit Tests Result Directory}
\usage{
  utestPath(...)
}
\arguments{
  \item{...}{extra arguments passed to
  \code{\link{packagePath}}, e.g., \code{package}.}
}
\description{
  Returns the path to the directory where the results of
  unit tests are stored. This path is used by
  \code{\link{utest}} to save unit test results, which are
  read by \code{\link{makeUnitVignette}} to update the unit
  test vignette when runnning R CMD check.
}

