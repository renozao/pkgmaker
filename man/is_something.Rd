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

  \code{is_NA} tests if a variable is exactly NA (logical,
  character, numeric or integer)

  \code{isFALSE} Tests if a variable is exactly FALSE.

  \code{isNumber} tests if a variable is a single number

  \code{isReal} tests if a variable is a single real number

  \code{isInteger} tests if an object is a single integer

  \code{isString} tests if an object is a character string.

  \code{is.dir} tests if a filename is a directory.

  \code{is.file} tests if a filename is a file.

  \code{hasNames} tests if an object has names.
}
\seealso{
  \code{\link{isTRUE}}
}

