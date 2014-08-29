\name{latex_preamble}
\alias{latex_bibliography}
\alias{latex_preamble}
\title{LaTeX Utilities for Vignettes}
\usage{
  latex_preamble(PACKAGE, R = TRUE, CRAN = TRUE,
    Bioconductor = TRUE, GEO = TRUE, ArrayExpress = TRUE,
    biblatex = FALSE, only = FALSE, file = "")

  latex_bibliography(PACKAGE, file = "")
}
\arguments{
  \item{R}{logical that indicate if general R commands should be added
(e.g. package names, inline R code format commands)}

  \item{CRAN}{logical that indicate if general CRAN commands should be added
(e.g. CRAN package citations)}

  \item{Bioconductor}{logical that indicate if general Bioconductor commands
should be added (e.g. Bioc package citations)}

  \item{GEO}{logical that indicate if general GEOmnibus commands should be added
(e.g. urls to GEO datasets)}

  \item{ArrayExpress}{logical that indicate if general ArrayExpress commands
should be added (e.g. urls to ArrayExpress datasets)}

  \item{biblatex}{logical that indicates if a \code{\\bibliography} command
should be added to include references from the package's REFERENCES.bib file.}

  \item{only}{a logical that indicates if the only the commands whose
dedicated argument is not missing should be considered.}

  \item{file}{connection where to print. If \code{NULL} the result is returned
silently.}

  \item{PACKAGE}{package name}
}
\description{
  \code{latex_preamble} outputs/returns command definition
  LaTeX commands to be put in the preamble of vignettes.

  \code{latex_bibliography} prints or return a LaTeX
  command that includes a package bibliography file if it
  exists.
}
\details{
  Argument \code{PACKAGE} is not required for
  \code{latex_preamble}, but must be correctly specified to
  ensure \code{biblatex=TRUE} generates the correct
  bibliography command.
}
\examples{
\dontshow{# roxygen generated flag
options(R_CHECK_RUNNING_EXAMPLES_=TRUE)
}

latex_preamble()
latex_preamble(R=TRUE, only=TRUE)
latex_preamble(R=FALSE, CRAN=FALSE, GEO=FALSE)
latex_preamble(GEO=TRUE, only=TRUE)
}

