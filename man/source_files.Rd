\name{source_files}
\alias{source_files}
\title{Source Multiple Files}
\usage{
  source_files(x, pattern = NULL, ...)
}
\arguments{
  \item{x}{character vector containing filenames}

  \item{...}{extra arguments passed to \code{\link{source}}.}

  \item{pattern}{an optional \link{regular expression}.  Only file names
    which match the regular expression will be returned.}
}
\description{
  Vectorised version of \code{source}.
}

