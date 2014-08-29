\name{addToLogger}
\alias{addToLogger}
\title{Enhancing RUnit Logger}
\usage{
  addToLogger(name, value, logger = NULL)
}
\arguments{
  \item{name}{name of the function or variable to add}

  \item{value}{object to append to the logger.
If \code{value} is a function it is added to the list and is accessible via
\code{.testLogger$name}.
If \code{value} is a variable it is added to the local environment and is
therefore accessible in all logging functions.}

  \item{logger}{an optional RUnit logger object.
If missing or \code{NULL}, the object \code{.testLogger} is searched in
\code{.GlobalEnv} -- and an error is thrown if it does not exist.}
}
\value{
  the modified logger object. Note that the global object is also
modified if \code{logger} is \code{NULL}.
}
\description{
  Adds a function or a local variable to RUnit global
  logger.
}

