\name{install.dependencies}
\alias{install.dependencies}
\title{Installing All Package Dependencies}
\usage{
  install.dependencies(pkg = NULL, all = FALSE, ...,
    dryrun = FALSE)
}
\arguments{
  \item{pkg}{package path or source file}

  \item{all}{logical that indicates if 'Suggests' packages
  should be installed.}

  \item{...}{extra arguments passed to
  \code{\link{install.packages}}.}

  \item{dryrun}{logical that indicates if the packages
  should be effectively installed or only shown.}
}
\description{
  Install all dependencies from a package source directory
  or package source file.
}
\examples{
try( install.dependencies('Matrix', dryrun=TRUE) )
\dontrun{
install.dependencies("mypackage_1.0.tar.gz", dryrun=TRUE)
}
}

