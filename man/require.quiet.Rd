\name{require.quiet}
\alias{require.quiet}
\title{Silent Require}
\usage{
  require.quiet(package, character.only = FALSE, ...)
}
\arguments{
  \item{...}{extra arguments passed to
  \code{\link{require}}.}

  \item{package}{the name of a package, given as a
  \link{name} or literal character string, or a character
  string, depending on whether \code{character.only} is
  \code{FALSE} (default) or \code{TRUE}).}

  \item{character.only}{a logical indicating whether
  \code{package} or \code{help} can be assumed to be
  character strings.}
}
\description{
  Silently require a package.
}

