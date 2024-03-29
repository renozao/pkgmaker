% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{charmap}
\alias{charmap}
\title{Substituting Strings Against a Mapping Table}
\usage{
charmap(x, maps, nomatch = NULL, partial = FALSE, rev = FALSE)
}
\arguments{
\item{x}{character vector to match}

\item{maps}{mapping tables.
May be a character vector, a list of character vectors or a function.}

\item{nomatch}{character string to be used for non-matched elements of \code{x}.
If \code{NULL}, these elements are left unchanged.}

\item{partial}{logical that indicates if partial matches are allowed,
in which case mappings are used as regular expressions.}

\item{rev}{logical that indicates if the mapping should be interpreted in the
reverse way.}
}
\value{
A character vector.
}
\description{
Match the elements of a character vectors against a mapping table,
that can specify multiple exact or partial matches.
}
