\encoding{utf8}
\name{write.bib}
\alias{write.bib}
\title{Generate a Bibtex File from Package Citations}
\usage{
  write.bib(entry = NULL, file = "Rpackages.bib",
    append = FALSE, verbose = TRUE)
}
\arguments{
  \item{entry}{a \code{\link{bibentry}} object or a
  character vector of package names. If \code{NULL}, then
  the list of all installed packages is used.}

  \item{file}{output Bibtex file. It can be specified as a
  filename (as a single character string), NULL for
  \code{stdout}, or a \code{link{connection}} object. If
  \code{file} is a character string, an extension '.bib' is
  appended if not already present.}

  \item{append}{a logical that indicates that the Bibtex
  entries should be added to the file. If \code{FALSE}
  (default), the file is overwritten.}

  \item{verbose}{a logical to toggle verbosity. If
  \code{file=NULL}, verbosity is forced off.}
}
\value{
  the list of Bibtex objects -- invisibly.
}
\description{
  Generates a Bibtex file from a list of packages or all
  the installed packages. It is useful for adding relevant
  citations in Sweave documents.
}
\details{
  Multiple citations are handled by adding a numeric suffix
  to the Bibtex key (other than the first/main citation) as
  \code{"<pkgname>\%i"} (e.g. pkg, pkg2, pkg3).

  This function has now been integrated by Romain François
  in the bibtex package.
}
\examples{
#library(bibtex)
pkgmaker::write.bib(c('bibtex', 'utils', 'tools'), file='references')
bibs <- bibtex::read.bib('references.bib')
pkgmaker::write.bib(bibs, 'references2.bib')
md5 <- tools::md5sum(c('references.bib', 'references2.bib'))
md5[1] == md5[2]
\dontshow{ stopifnot(md5[1] == md5[2]) }

# write to stdout()
pkgmaker::write.bib(c('bibtex', 'utils', 'tools'), file=NULL)

\dontshow{ unlink(c('references.bib', 'references2.bib'))}
}
\author{
  Renaud Gaujoux, based on the function
  \code{Rpackages.bib} from Achim Zeileis (see
  \emph{References}).
}
\references{
  \emph{[R] Creating bibtex file of all installed
  packages?} Achim Zeileis. R-help mailing list.
  \url{https://stat.ethz.ch/pipermail/r-help/2009-December/222201.html}
}
\seealso{
  \code{link{connection}}, \code{link{bibentry}}
}

