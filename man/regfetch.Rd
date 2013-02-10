\name{regfetch}
\alias{pkgregfetch}
\alias{regfetch}
\title{Finds an entry in a registry.}
\usage{
  regfetch(regobj, key = NULL, all = FALSE, error = TRUE,
    exact = FALSE, verbose = FALSE, entry = FALSE,
    msg = NULL)

  pkgregfetch(name, ..., msg = NULL,
    package = topenv(parent.frame()))
}
\arguments{
  \item{regobj}{a registry object}

  \item{key}{a key to match}

  \item{all}{logical to indicate if hidden keys (starting
  with a '.') should be returned and output in message.}

  \item{error}{a logical that indicates if an error should
  be thrown if the key has no match or multiple matches}

  \item{exact}{a logical that indicates if matching should
  be exact or partial}

  \item{verbose}{a logical that indicates if verbosity
  should be toggle on}

  \item{entry}{a logical that indicates if the}

  \item{msg}{a header to use in case of error.}

  \item{name}{Name of a sub-registry, used as its
  identifier.}

  \item{...}{named values used to set extra information
  about the new registry, that are stored in the
  corresponding fields of the meta-registry. Currently not
  used, as no extra field other than \code{'description'}
  is defined.}

  \item{package}{package where to store or look for the
  registry.}
}
\description{
  This function provides extra control on how entries are
  queried from a \code{\link[registry:regobj]{registry}}
  object.

  \code{pkgregfetch} fetches entries in a package registry,
  as set up by \code{\link{setPackageRegistry}}
}

