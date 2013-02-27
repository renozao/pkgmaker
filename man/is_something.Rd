\name{is_something}
\alias{hasNames}
\alias{is.dir}
\alias{isFALSE}
\alias{is.file}
\alias{isInteger}
\alias{is_NA}
\alias{isNumber}
\alias{isReal}
\alias{is_something}
\alias{isString}
\title{Testing Object Type}
\usage{
  is_NA(x)

  isFALSE(x)

  isNumber(x)

  isReal(x)

  isInteger(x)

  isString(x, y, ignore.case = FALSE)

  is.dir(x)

  is.file(x)

  hasNames(x, all = FALSE)
}
\arguments{
  \item{x}{an R object}

  \item{y}{character string to compare with.}

  \item{ignore.case}{logical that indicates if the
  comparison should be case sensistive.}

  \item{all}{logical that indicates if the object needs all
  names non empty}
}
\value{
  \code{TRUE} or \code{FALSE}
}
\description{
  Testing Object Type

  Tests if a variable is exactly NA (logical, character,
  numeric or integer)

  Tests if a variable is exactly FALSE

  Tests if a variable is a single number

  Tests if a variable is a single real number

  Tests if an object is a single integer

  \code{isString} tests if an object is a character string.

  Tests if a filename is a directory

  Tests if a filename is a file

  Tests if an object has names
}
\seealso{
  \code{\link{isTRUE}}
}

