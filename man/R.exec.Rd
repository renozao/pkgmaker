\name{R.exec}
\alias{R.CMD}
\alias{R.exec}
\alias{R.SHLIB}
\title{Executing R Commands}
\usage{
  R.exec(...)

  R.CMD(cmd, ...)

  R.SHLIB(libname, ...)
}
\arguments{
  \item{...}{extra arguments that are concatenated and
  appended to the command.}

  \item{cmd}{command to run, e.g. \sQuote{check} or
  \sQuote{INSTALL}.}

  \item{libname}{name of the output compiled library}
}
\description{
  \code{R.exec} executes a single R command via
  \code{\link{system2}}.

  \code{R.CMD} executes R CMD commands.

  \code{R.SHLIB} executes R CMD SHLIB commands.
}

