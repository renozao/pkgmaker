\name{require.quiet}
\alias{mrequire}
\alias{qlibrary}
\alias{qrequire}
\alias{require.quiet}
\title{Loading Packages}
\usage{
  require.quiet(...)

  qrequire(...)

  qlibrary(...)

  mrequire(msg, package, ...)
}
\arguments{
  \item{...}{extra arguments passed to \code{\link{library}} or \code{\link{require}}.}

  \item{msg}{error message to use, to which is appended the string
\code{' requires package <pkg>'} to build the error message.}

  \item{package}{name of the package to load.}
}
\description{
  \code{require.quiet} silently requires a package, and
  \code{qrequire} is an alias to \code{require.quiet}.

  \code{qlibrary} silently loads a package.

  \code{mrequire} tries loading a package with base
  \code{\link{require}} and stops with a -- custom -- error
  message if it fails to do so.
}
\examples{
\dontshow{# roxygen generated flag
options(R_CHECK_RUNNING_EXAMPLES_=TRUE)
}

mrequire('Running this example', 'stringr')
try( mrequire('Doing impossible things', 'notapackage') )
}
\seealso{
  Other require: \code{\link{irequire}}
}

