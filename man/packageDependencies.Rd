\name{packageDependencies}
\alias{packageDependencies}
\title{List Package Dependencies}
\usage{
  packageDependencies(x, all = TRUE, as.list = FALSE,
    available = NULL)
}
\arguments{
  \item{x}{path to package source directory or file.}

  \item{all}{logical that indicates if all dependencies
  should be returned, or only the required ones.}

  \item{as.list}{logical that indicates if the result
  should be a list with one element per type of
  dependency.}

  \item{available}{a matrix of available packages (as
  returned by \code{\link{available.packages}}), from which
  the dependencies are retrieved. This means that there
  must be a row for the package \code{x}.}
}
\description{
  List Package Dependencies
}

