\name{utestFramework}
\alias{utestFramework}
\title{Inferring Unit Test Framework}
\usage{
  utestFramework(x, eval = FALSE)
}
\arguments{
  \item{x}{an filename, a function or the body of a function}

  \item{eval}{a logical that indicates if the value of \code{x} should be used.}
}
\value{
  the name of the framework as a character string or NULL if
it could not be detected.
}
\description{
  Inferring Unit Test Framework
}

