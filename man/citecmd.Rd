\name{citecmd}
\alias{citecmd}
\title{Citing Package References}
\usage{
  citecmd(key, ..., REFERENCES = NULL)
}
\arguments{
  \item{key}{character vector of BibTex keys}

  \item{...}{extra arguments passed to
  \code{format.bibentry}.}

  \item{REFERENCES}{package or bibentry specification}
}
\value{
  a character string containing the text formated BibTex
  entries
}
\description{
  Create a citation command from package specific BibTex
  entries, suitable to be used in Rd files or Latex
  documents. The entries are looked in a file named
  REFERNCES.bib in the package's root directory (i.e. inst/
  in development mode).
}

