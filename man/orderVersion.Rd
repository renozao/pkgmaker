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
\dontshow{# roxygen generated flag
options(R_CHECK_RUNNING_EXAMPLES_=TRUE)
}

#----------
# orderVersion
#----------
v <- c('1.0', '1.03', '1.2')
order(v)
orderVersion(v)

#----------
# sortVersion
#----------
sort(v)
sortVersion(v)
}

