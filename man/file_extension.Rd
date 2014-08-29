\name{file_extension}
\alias{file_extension}
\title{Extract File Extension}
\usage{
  file_extension(x, ext = NULL)
}
\arguments{
  \item{x}{path as a character vector.}

  \item{ext}{extension to append instead of the original extension.}
}
\description{
  Extract File Extension
}
\examples{
\dontshow{# roxygen generated flag
options(R_CHECK_RUNNING_EXAMPLES_=TRUE)
}

file_extension('alpha.txt')
file_extension(paste('aa.tt', 1:5, sep=''))
# change extension
file_extension(paste('aa.tt', 1:5, sep=''), 'pdf')
file_extension(paste('aatt', 1:5, sep=''), 'pdf')
}

