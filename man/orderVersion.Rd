\name{orderVersion}
\alias{orderVersion}
\alias{sortVersion}
\title{Ordering Version Numbers}
\usage{
  orderVersion(x, decreasing = FALSE)

  sortVersion(x, ...)
}
\arguments{
  \item{x}{a character vector of version numbers}

  \item{decreasing}{a logical that indicates if the
  ordering should be decreasing}

  \item{...}{extra parameters passed to
  \code{orderVersion}}
}
\description{
  Orders a vector of version numbers, in natural order.
}
\examples{
v <- c('1.0', '1.03', '1.2')
order(v)
orderVersion(v)
sort(v)
sortVersion(v)
}

