\name{getLoadingNamespace}
\alias{addNamespaceExport}
\alias{getLoadingNamespace}
\alias{isDevNamespace}
\alias{isLoadingNamespace}
\alias{isNamespaceLoaded}
\alias{ns_get}
\title{Namespace Development Functions}
\usage{
  getLoadingNamespace(env = FALSE, info = FALSE,
    nodev = FALSE)

  isLoadingNamespace(ns, nodev = FALSE)

  isNamespaceLoaded(ns)

  isDevNamespace(ns)

  addNamespaceExport(x)

  ns_get(x, ns)
}
\arguments{
  \item{env}{logical that indicates that the namespace's environment (i.e. the
namespace itself) should be returned.}

  \item{info}{logical that indicates that the complete information list should
be returned}

  \item{ns}{the name of a namespace or a namespace whose loading state is tested.
If missing \code{isLoadingNamespace} test if any namespace is being loaded.}

  \item{nodev}{logical that indicates if loading devtools namespace should
be discarded.}

  \item{x}{character vector containing the names of R objects to export in the
loading namespace.}
}
\value{
  the name of the loading namespace if \code{env} and \code{info} are
\code{FALSE}, an environment if \code{env=TRUE}, a list with elements
\code{pkgname} and \code{libname} if \code{info=TRUE}.
}
\description{
  \code{getLoadingNamespace} returns information about the
  loading namespace. It is a wrapper to
  \code{\link{loadingNamespaceInfo}}, that does not throw
  an error.

  Tests if a namespace is being loaded.

  \code{isNamespaceLoaded} tests if a given namespace is
  loaded, without loading it, contrary to
  \code{\link{isNamespace}}.

  \code{isDevNamespace} tests the -- current -- namespace
  is a devtools namespace.

  Dynamically adds exported objects into the loading
  namespace.

  \code{ns_get} gets an object from a given namespace.
}

