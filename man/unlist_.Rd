% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{unlist_}
\alias{unlist_}
\title{Flatten a List Conserving Names}
\source{
Bioconductor AnnotationDbi::unlist2
}
\usage{
unlist_(x, recursive = TRUE, use.names = TRUE, what.names = "inherited")
}
\arguments{
\item{x, recursive, use.names}{See \code{?unlist}.}

\item{what.names}{\code{"inherited"} or \code{"full"}.}
}
\value{
A vector.
}
\description{
\code{unlist_} is a replacement for \link[base:unlist]{base::unlist} that does not mangle the names.
}
\details{
Use this function if you don't like the mangled names returned by the standard \code{unlist} function from the base package.
Using \code{unlist} with annotation data is dangerous and it is highly recommended to use \code{unlist_} instead.
}
\examples{
x <- list(A=c(b=-4, 2, b=7), B=3:-1, c(a=1, a=-2), C=list(c(2:-1, d=55), e=99))
unlist(x)
unlist_(x)

# annotation maps (as in AnnotationDbi objects
egids2pbids <- list('10' = 'a', '100' = c('b', 'c'), '1000' = c('d', 'e'))
egids2pbids

unlist(egids2pbids)   # 1001, 1002, 10001 and 10002 are not real
                      # Entrez ids but are the result of unlist()
                      # mangling the names!
unlist_(egids2pbids)  # much cleaner! yes the names are not unique
                      # but at least they are correct...

}
\author{
Herve Pages
}
