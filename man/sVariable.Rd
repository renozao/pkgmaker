\name{sVariable}
\alias{sVariable}
\title{Global Static Variable}
\usage{
  sVariable(default = NULL)
}
\arguments{
  \item{default}{default value for the static variable.}
}
\description{
  \code{sVariable} defines a function that acts as a global
  static variable.
}
\examples{
# define variable
x <- sVariable(1)
# get value (default)
x()
# set new value: return old value
old <- x(3)
old
# get new value
x()
}

