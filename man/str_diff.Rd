\name{str_diff}
\alias{str_diff}
\title{Finding Differences Between Strings}
\usage{
  str_diff(x, y)
}
\arguments{
  \item{x}{a single string}

  \item{y}{a single string}
}
\value{
  an integer vector containing the index of all mis-matched characters
in the first string.
}
\description{
  Computes which characters differ between two strings.
}
\examples{
\dontshow{# roxygen generated flag
options(R_CHECK_RUNNING_EXAMPLES_=TRUE)
}

# strings to compare
x <- "once upon a time"
y <- "once upon a time there was"
z <- "once upon two times"

# diff: x - y
d <- str_diff(x, y)
d
str(d)

# other comparisons
str_diff(y, x)
str_diff(x, x)
str_diff(x, z)
str_diff(y, z)
}

