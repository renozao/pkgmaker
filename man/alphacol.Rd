\name{alphacol}
\alias{alphacol}
\title{Colour utilities}
\usage{
  alphacol(col, alpha = FALSE)
}
\arguments{
  \item{col}{vector of any of the three kind of \R colors,
  i.e., either a color name (an element of
  \code{\link{colors}()}), a hexadecimal string of the form
  \code{"#rrggbb"} or \code{"#rrggbbaa"} (see
  \code{\link{rgb}}), or an integer \code{i} meaning
  \code{palette()[i]}.  Non-string values are coerced to
  integer.}

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

