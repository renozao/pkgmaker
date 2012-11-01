\name{regfetch}
\alias{regfetch}
\title{Finds an entry in a registry.}
\usage{
  regfetch(regobj, key = NULL, msg = NULL, all = FALSE,
    error = TRUE, exact = FALSE, verbose = FALSE)
}
\arguments{
  \item{regobj}{a registry object}

  \item{key}{a key to match}

  \item{msg}{a header to use in case of error}

  \item{all}{logical to indicate if hidden keys (starting
  with a '.') should be returned and output in message.}

  \item{error}{a logical that indicates if an error should
  be thrown if the key has no match or multiple matches}

  \item{exact}{a logical that indicates if matching should
  be exact or partial}

  \item{verbose}{a logical that indicates if verbosity
  should be toggle on}
}
\description{
  Finds an entry in a registry.
}

