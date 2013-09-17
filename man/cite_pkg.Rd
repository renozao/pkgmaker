\name{cite_pkg}
\alias{cite_pkg}
\title{\code{bibcite} provides an inline package citation functionnality.
Technically it adds a given Bibtex key to a cache that is used at the end of the
document processing to generate a .bib file with all citation keys.}
\usage{
  cite_pkg(key, cache = NA, ...)
}
\arguments{
  \item{key}{citation Bibtex key(s) as a character vector}

  \item{cache}{specifies what to do with the previsouly
  chached keys. If \code{TRUE}, then \code{key} is added to
  the cache. If \code{NULL}, then all previously cached
  keys are deleted, before . If a character string, then it
  specifies the path to a Bibtex file that is loaded to
  initialise the cache.}

  \item{...}{extra arguments passed to
  \code{\link[bibtex]{read.bib}}.}
}
\description{
  \code{bibcite} provides an inline package citation
  functionnality. Technically it adds a given Bibtex key to
  a cache that is used at the end of the document
  processing to generate a .bib file with all citation
  keys.
}
\keyword{internal}

