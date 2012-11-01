\name{str_out}
\alias{str_desc}
\alias{str_out}
\title{Prints formatted list of values given as a character vector for use in show
methods or error/warning messages.}
\usage{
  str_out(x, max = 3L, quote = is.character(x),
    use.names = FALSE, sep = ", ")

  str_desc(object, exdent = 0L)
}
\arguments{
  \item{x}{character vector}

  \item{max}{maximum number of values to appear in the
  list. If \code{x} has more elements than \code{max}, a
  \code{"..."} suffix is appended.}

  \item{quote}{a logical indicating whether the values
  should be quoted with single quotes (defaults) or not.}

  \item{use.names}{a logical indicating whether names
  should be added to the list as \code{NAME=VAL, ...} or
  not (default).}

  \item{sep}{separator character}

  \item{object}{an R object}

  \item{exdent}{extra indentation passed to str_wrap, and
  used if the output should spread over more than one
  lines.}
}
\value{
  a single character string
}
\description{
  Prints formatted list of values given as a character
  vector for use in show methods or error/warning messages.

  Builds formatted string from a list of complex values.
}
\examples{
x <- letters[1:10]
str_out(x)
str_out(x, 8)
str_out(x, Inf)
str_out(x, quote=FALSE)
}

