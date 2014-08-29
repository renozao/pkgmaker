\name{winbuild}
\alias{winbuild}
\title{Build a Windows Binary Package}
\usage{
  winbuild(path, outdir = ".", verbose = TRUE)
}
\arguments{
  \item{path}{path to a source or already installed package}

  \item{outdir}{output directory}

  \item{verbose}{logical or numeric that indicates the verbosity level}
}
\value{
  Invisibly returns the full path to the generated zip file.
}
\description{
  Build a Windows Binary Package
}
\examples{
\dontshow{# roxygen generated flag
options(R_CHECK_RUNNING_EXAMPLES_=TRUE)
}

\dontrun{

# from source directory
winbuild('path/to/package/source/dir/')
# from tar ball
winbuild('PKG_1.0.tar.gz')

}
}

