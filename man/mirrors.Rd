\docType{data}
\name{setBiocMirror}
\alias{CRAN}
\alias{getBiocMirror}
\alias{getBiocRepos}
\alias{setBiocMirror}
\alias{setCRANMirror}
\title{Setting Mirrors and Repositories}
\format{chr "http://cran.r-project.org"}
\usage{
  setBiocMirror(url = "http://www.bioconductor.org",
    version = NULL, unique = TRUE)

  getBiocMirror()

  getBiocRepos(url = "http://www.bioconductor.org",
    version = NULL)

  setCRANMirror(url = CRAN, unique = TRUE)

  CRAN
}
\arguments{
  \item{url}{or Bioconductor mirror url}

  \item{version}{version number}

  \item{unique}{logical that indicate if duplicated urls or names should be
removed.}
}
\description{
  \code{setBiocMirror} sets all Bioconductor repositories
  (software, data, annotation, etc.). so that they are
  directly available to \code{\link{install.packages}}. It
  differs from \code{\link{chooseBioCmirror}} in that it
  effectively enables the repositories.

  \code{getBiocMirror} is a shortcut for
  \code{getOption('BioC_mirror')}, which returns the
  current Bioconductor mirror as used by \code{biocLite}.

  \code{getBiocRepos} returns urls to all Bioconductor
  repositories on a given mirror.

  \code{setCRANMirror} sets the preferred CRAN mirror.

  \code{CRAN} simply contains the url of CRAN main mirror
  (\url{http://cran.r-project.org}), and aims at
  simplifying its use, e.g., in calls to
  \code{\link{install.packages}}.
}
\examples{
\dontshow{# roxygen generated flag
options(R_CHECK_RUNNING_EXAMPLES_=TRUE)
}

\dontrun{
install.packages('pkgmaker', repos=CRAN)
}
}
\keyword{datasets}

