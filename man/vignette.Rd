\name{isManualVignette}
\alias{as.rnw}
\alias{compactVignettes}
\alias{isManualVignette}
\alias{rnw}
\alias{rnwChildren}
\alias{rnwCompiler}
\alias{rnwDriver}
\alias{rnwIncludes}
\alias{rnwWrapper}
\alias{vignetteMakefile}
\title{Identifies Manually Run Vignettes}
\usage{
  isManualVignette()

  rnw(x, file = NULL, ..., raw = FALSE)

  as.rnw(x, ..., load = TRUE)

  rnwCompiler(x, verbose = TRUE)

  rnwWrapper(x, verbose = TRUE)

  rnwDriver(x)

  rnwIncludes(x)

  rnwChildren(x)

  vignetteMakefile(package = NULL, skip = NULL,
    print = TRUE, template = NULL, temp = FALSE,
    checkMode = isCHECK() || vignetteCheckMode(),
    user = NULL, tests = TRUE)

  compactVignettes(paths, ...)
}
\arguments{
  \item{x}{vignette source file specification as a path or
  a \code{rnw} object.}

  \item{file}{output file}

  \item{...}{extra arguments passed to \code{as.rnw} that
  can be used to force certain building parameters.}

  \item{raw}{a logical that indicates if the raw result for
  the compilation should be returned, instead of the result
  file path.}

  \item{load}{logical to indicate if all the object's
  properties should loaded, which is done by parsing the
  file and look up for specific tags.}

  \item{verbose}{logical that toggles verbosity}

  \item{package}{package name. If \code{NULL}, a DESRIPTION
  file is looked for one directory up: this meant to work
  when building a vignette directly from a package's
  \code{'vignettes'} sub-directory.}

  \item{skip}{Vignette files to skip (basename).}

  \item{print}{logical that specifies if the path should be
  printed or only returned.}

  \item{template}{template Makefile to use. The default is
  to use the file \dQuote{vignette.mk} shipped with the
  package \pkg{pkgmaker} and can be found in its install
  root directory.}

  \item{temp}{logical that indicates if the generated
  makefile should using a temporary filename (\code{TRUE}),
  or simply named \dQuote{vignette.mk}}

  \item{checkMode}{logical that indicates if the vignettes
  should be generated as in a CRAN check (\code{TRUE}) or
  in development mode, in which case \code{pdflatex},
  \code{bibtex}, and, optionally, \code{qpdf} are
  required.}

  \item{user}{character vector containing usernames that
  enforce \code{checkMode=TRUE}, if the function is called
  from within their session.}

  \item{tests}{logical that enables the compilation of a
  vignette that gathers all unit test results. Note that
  this means that all unit tests are run before generating
  the vignette. However, unit tests are not (re)-run at
  this stage when the vignettes are built when checking the
  package with \code{R CMD check}.}

  \item{paths}{A character vector of paths to PDF files, or
  a length-one character vector naming a directory, when
  all \file{.pdf} files in that directory will be used.}
}
\description{
  \code{isManualVignette} tells if a vignette is being run
  through the function \code{runVignette} of \pkg{pkgmker},
  allowing disabling behaviours not allowed in package
  vignettes that are checked vi \code{R CMD check}.

  \code{rnw} provides a unified interface to run vignettes
  that detects the type of vignette (Sweave or
  \code{\link[knitr]{knitr}}), and which Sweave driver to
  use (either automatically or from an embedded command
  \code{\\VignetteDriver} command).

  \code{as.rnw} creates a S3 \code{rnw} object that
  contains information about a vignette, e.g., source
  filename, driver, fixed included files, etc..

  \code{rnwCompiler} tries to detect the vignette compiler
  to use on a vignette source file, e.g.,
  \code{\link{Sweave}} or \code{\link[knitr]{knitr}}.

  \code{rnwWrapper} tries to detect the type of vignette
  and if it is meant to be wrapped into another main file.

  \code{rnwDriver} tries to detect Sweave driver to use on
  a vignette source file, e.g., \code{SweaveCache},
  \code{highlight}, etc..

  \code{rnwIncludes} detects fixed includes, e.g., image or
  pdf files, that are required to build the final document.

  \code{rnwChildren} detects included vignette documents
  and return them as a list of vignette objects.

  \code{vignetteMakefile} returns the path to a generic
  makefile used to make vignettes.

  Compact PDFs using either \code{gs_quality='none'} or
  \code{'ebook'}, depending on which compacts best (as per
  CRAN check criteria).
}

