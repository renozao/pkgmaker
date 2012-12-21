\name{quickinstall}
\alias{quickinstall}
\title{Quick Installation of a Source Package}
\usage{
  quickinstall(path, destdir = NULL, vignettes = FALSE,
    ...)
}
\arguments{
  \item{path}{path to the package source directory}

  \item{destdir}{installation directory. Defaults to the
  user's R default installation directory.}

  \item{vignettes}{logical that indicates if the vignettes
  should be rebuilt and installed.}

  \item{...}{extra arguments passed to \code{\link{R.CMD}}
  (e.g., \var{lib.loc})}
}
\description{
  Builds and install a minimal version of a package from
  its source directory.
}

