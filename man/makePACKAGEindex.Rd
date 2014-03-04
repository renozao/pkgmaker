\name{makePACKAGEindex}
\alias{makePACKAGEindex}
\title{Generate CRAN-like Repository Index}
\usage{
  makePACKAGEindex(path = ".", output = "index.html",
    pattern = NULL, title = "Packages", robots.file = TRUE)
}
\arguments{
  \item{path}{path to the repository's root directory}

  \item{output}{output filename -- relative to the
  repository root \code{path}.}

  \item{pattern}{regular expression used to filter the
  names of the packages that will appear in the index.}

  \item{title}{title of the index page}

  \item{robots.file}{logical that indicates if a file
  \code{robots.txt} that hides the repository from search
  engine robots should be created.}
}
\description{
  Generate CRAN-like Repository Index
}

