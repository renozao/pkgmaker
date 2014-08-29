\name{addnames}
\alias{addnames}
\alias{addnames.array}
\alias{addnames.default}
\alias{addnames.matrix}
\alias{addnames.vector}
\title{Generating Names}
\usage{
  addnames(x, ...)

  \method{addnames}{default} (x, ...)

  \method{addnames}{vector} (x, prefix = "x", sep = "",
    ...)

  \method{addnames}{array} (x,
    prefix = letters[1:length(dim(x))], sep = "", ...)

  \method{addnames}{matrix} (x, prefix = c("row", "col"),
    ...)
}
\arguments{
  \item{x}{object whose names are generated.}

  \item{prefix}{prefix string to use. A vector can be used to specify a prefix
for each dimension of \code{x}.
Names are build as \code{<prefix><sep><index>}.}

  \item{sep}{separator used between the prefix and the numeric index.}

  \item{...}{extra arguments to allow extension and passed to the next method.}
}
\description{
  Generates names or dimnames for objects.
}

