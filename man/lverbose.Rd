\name{lverbose}
\alias{is.verbose}
\alias{lmessage}
\alias{lsilent}
\alias{lverbose}
\alias{vmessage}
\title{Internal verbosity option}
\usage{
  lverbose(val)

  lsilent()

  is.verbose()

  vmessage(...)

  lmessage(..., level = 1L, appendLF = TRUE)
}
\arguments{
  \item{val}{logical that sets the verbosity level.}

  \item{...}{arguments passed to \code{...} \code{\link{cat}}}

  \item{appendLF}{logical indicating if an endline character should be appended
at the end of the message. Passed to \code{\link{cat}}.}

  \item{level}{verbosity level threshold (numeric value) above which the
message should be printed out.
This threshold is compared with the current verbosity level as returned by
\code{lverbose}.}

  \item{...}{arguments passed to \code{...} \code{\link{lmessage}} or \code{\link{cat}}}

  \item{appendLF}{logical indicating if an endline character should be appended
at the end of the message. Passed to \code{\link{cat}}.}
}
\value{
  the old verbose level
}
\description{
  Internal verbosity option

  Tells if all verbose messages should be

  Tells if verbosity is on.

  Prints out a message (on sdtout) if verbose mode is on.

  Prints out a message (on sdtout) if the verbosity level
  is greater than a given value.
}
\keyword{internal}

