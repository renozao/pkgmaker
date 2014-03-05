\name{onLoad}
\alias{onLoad}
\alias{onUnload}
\title{Default Load/Unload Functions}
\usage{
  onLoad(libname = NULL, pkgname, chname = packageName())

  onUnload(libpath)
}
\arguments{
  \item{libname}{a character string giving the library
  directory where the package defining the namespace was
  found.}

  \item{pkgname}{a character string giving the name of the
  package.}

  \item{libpath}{a character string giving the complete
  path to the package.}

  \item{chname}{a character string naming a DLL (also known
  as a dynamic shared object or library) to load.}
}
\description{
  Default Load/Unload Functions
}
\examples{
\dontshow{# roxygen generated flag
options(R_CHECK_RUNNING_EXAMPLES_=TRUE)
}

#----------
# onLoad
#----------
# in a package namespace:
.onLoad <- function(libname=NULL, pkgname){

	pkgmaker::onLoad(libname, pkgname)

}

#----------
# onUnload
#----------
# in a package namespace:
.onUnload <- function(libpath){

	pkgmaker::onUnload(libpath)

}
}

