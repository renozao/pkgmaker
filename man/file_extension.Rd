% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/files.R
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
\value{
a character vector.
}
\description{
Extract File Extension
}
\examples{

file_extension('alpha.txt')
file_extension(paste('aa.tt', 1:5, sep=''))
# change extension
file_extension(paste('aa.tt', 1:5, sep=''), 'pdf')
file_extension(paste('aatt', 1:5, sep=''), 'pdf')

}
