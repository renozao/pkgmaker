\name{list.libs}
\alias{libname}
\alias{list.libs}
\title{Library Files Utilities}
\usage{
  list.libs(dir, ..., all.platforms = FALSE)

  libname(x)
}
\arguments{
  \item{dir}{directory}

  \item{all.platforms}{a logical that indicates whether to
  list library files for the current platform only
  (default) or all platforms (Unix, Windows, Mac).}

  \item{...}{extra arguments passed to
  \code{\link{list.files}}.}

  \item{x}{a filename}
}
\value{
  a character vector
}
\description{
  Lists binary library files in a directory

  \code{libname} extracts library names from a path,
  removing the directory part of the path, as well as the
  platform specific library extension.
}
\examples{
libname('mylib.so')
libname('/some/path/somewhere/mylib.dll')
}

