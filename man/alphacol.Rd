\name{alphacol}
\alias{alphacol}
\title{Colour utilities}
\usage{
  alphacol(col, alpha = FALSE)
}
\arguments{
  \item{col}{vector of any of the three kinds of \R color
  specifications, i.e., either a color name (as listed by
  \code{\link{colors}()}), a hexadecimal string of the form
  \code{"#rrggbb"} or \code{"#rrggbbaa"} (see
  \code{\link{rgb}}), or a positive integer \code{i}
  meaning \code{\link{palette}()[i]} or \code{0} (see
  \sQuote{Details}).  Non-character values are coerced to
  integers.}

  \item{alpha}{logical value indicating whether alpha
  channel (opacity) values should be returned.}
}
\description{
  \code{alphacol} adds an alpha value to a colour
  specification and convert to a hexadecimal colour string.
}
\examples{
# Alphas
alphacol('red') # do nothing
alphacol('red', 10)
alphacol('#aabbcc', 5)
alphacol(4, 5)
}

