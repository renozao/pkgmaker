\name{quickinstall}
\alias{quickinstall}
\title{Quick Installation of a Source Package}
\usage{
  quickinstall(path, destdir = NULL, vignettes = FALSE,
    force = TRUE, ...,
    lib.loc = if (!is.null(destdir)) TRUE)
}
\arguments{
  \item{path}{path to the package source directory}

  \item{destdir}{installation directory. If \code{NULL},
  the package is installed in the default installation
  library. If \code{NA}, the package is installed in a
  temporary directory, whose path is returned as a value.}

  \item{vignettes}{logical that indicates if the vignettes
  should be rebuilt and installed.}

  \item{force}{logical that indicates if the package should
  be installed even if a previous installation exists in
  the installation library.}

  \item{...}{extra arguments passed to \code{\link{R.CMD}}}

  \item{lib.loc}{library specification. If \code{TRUE} then
  the installation directory \code{destdir} is added to the
  default library paths. This can be usefull if
  dependencies are installed in this directory. If
  \code{NULL}, then the default library path is left
  unchanged.}
}
\value{
  The path of the library where the package was installed.
}
\description{
  Builds and install a minimal version of a package from
  its source directory.
}

