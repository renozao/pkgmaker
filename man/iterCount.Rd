% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/logging.R
\name{iterCount}
\alias{iterCount}
\title{Simple Text Iteration Counter}
\usage{
iterCount(n = 100, i0 = 0L, title = "Iterations", extra = NULL, verbose = TRUE)
}
\arguments{
\item{n}{number of total steps}

\item{i0}{starting step}

\item{title}{character string to use as title}

\item{extra}{character vector providing extra text to add at each step}

\item{verbose}{logical that toggles the counter}
}
\value{
A function that can be used to increment progress.
}
\description{
Simple Text Iteration Counter
}
\examples{

progress <- iterCount(LETTERS)
res <- sapply(LETTERS, function(x){
 Sys.sleep(.1)
	progress()
})
# terminate counter
i_end <- progress(NULL)
i_end 

}
