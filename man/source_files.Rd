% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/files.R
\name{source_files}
\alias{source_files}
\title{Source Multiple Files}
\usage{
source_files(x, pattern = NULL, ...)
}
\arguments{
\item{x}{character vector containing filenames}

\item{pattern}{an optional \link[base]{regular expression}.  Only file names
    which match the regular expression will be returned.}

\item{...}{extra arguments passed to \code{\link{source}}.}
}
\value{
the return value of running \link{source} on each individual file.
}
\description{
Vectorised version of \code{source}.
}
