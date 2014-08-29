\name{oneoffVariable}
\alias{oneoffVariable}
\title{One-off Global Variables}
\usage{
  oneoffVariable(default = NULL)
}
\arguments{
  \item{default}{default value to which the global variable is reset after each
access. Default is \code{NULL}.}
}
\value{
  a function with one argument (\code{value}) that provides get/set access
to a global variable.
If called with a value, it assigns this value to the global variable.
If called with no argument, it returns the current value of the global variable and
reset it to its default value -- as defined at its creation.
}
\description{
  Defines a function that allow to get/assign a global
  variable whose value is ensured to be reset after each
  access.
}
\examples{
\dontshow{# roxygen generated flag
options(R_CHECK_RUNNING_EXAMPLES_=TRUE)
}

x <- oneoffVariable(0)
# returns default value
x()
# assign a value
x(3)
# get the value
x()
# second call returns default value again
x()
}

