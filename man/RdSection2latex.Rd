\name{RdSection2latex}
\alias{RdSection2latex}
\title{Format Rd Sections into LatTeX}
\usage{
  RdSection2latex(topic, package, i = 1L, notitle = TRUE)
}
\arguments{
  \item{topic}{Rd topic}

  \item{package}{package in which to search the topic}

  \item{i}{index of the section to format}

  \item{notitle}{logical that indicates if the section's
  title should be removed}
}
\description{
  This function extract sections from Rd files and convert
  them into LaTeX code. This can be useful to include Rd
  text into vignettes, hence keeping them up to date.
}
\section{Example section}{
  This is a nice section, with a bullet list: \itemize{
  \item tata \item toto }
}
\examples{
\dontshow{# roxygen generated flag
options(R_CHECK_RUNNING_EXAMPLES_=TRUE)
}

RdSection2latex('RdSection2latex', package = 'pkgmaker')
}

