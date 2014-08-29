\name{regfetch}
\alias{pkgreg_fetch}
\alias{pkgreg_remove}
\alias{regfetch}
\title{Finds an entry in a registry.}
\usage{
  regfetch(regobj, ..., all = FALSE, error = TRUE,
    exact = FALSE, KEYS = NULL, verbose = FALSE,
    entry = FALSE, msg = NULL)

  pkgreg_fetch(regname, ..., msg = NULL,
    where = topenv(parent.frame()))

  pkgreg_remove(regname, ..., msg = NULL,
    where = topenv(parent.frame()), quiet = FALSE)
}
\arguments{
  \item{regobj}{a registry object}

  \item{...}{key value(s) to look up.
If multiple indexes are used, then the primary key should come first.}

  \item{all}{logical to indicate if hidden keys (starting with a '.') should be
returned and output in message.}

  \item{error}{a logical that indicates if an error should be thrown if the key has no match
or multiple matches}

  \item{exact}{a logical that indicates if matching should be exact or partial.
Note that if exact matches exist then they are returned, independently of the
value of \code{exact}.}

  \item{KEYS}{alternative way of passing the key value(s).
If not missing, then arguments in \code{...} are discarded.}

  \item{verbose}{a logical that indicates if verbosity should be toggle on}

  \item{entry}{a logical that indicates if the}

  \item{msg}{a header to use in case of error.}

  \item{quiet}{a logical that indicates if the operation should be performed quietly,
without throwing errors or warnings.}

  \item{regname}{Name of a sub-registry, used as its identifier.}

  \item{where}{package name or namespace that owns the registry.}
}
\description{
  This function provides extra control on how entries are
  queried from a \code{\link[registry:regobj]{registry}}
  object.

  \code{pkgreg_fetch} loads the requested package registry
  and uses \code{regfetch} to retrieve data from it.

  \code{pkgreg_remove} removes an entry from a package
  registry.
}

