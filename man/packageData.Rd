\name{packageData}
\alias{ldata}
\alias{packageData}
\title{Loading Package Data}
\usage{
  packageData(list, envir = .GlobalEnv, ...)

  ldata(list, ...)
}
\arguments{
  \item{list}{character vector containing the names of the
  data to load.}

  \item{...}{other arguments eventually passed to
  \code{\link[utils]{data}}.}

  \item{envir}{the \link{environment} where the data should
  be loaded.}
}
\value{
  the loaded data.
}
\description{
  Loads package data using \code{\link[utils]{data}}, but
  allows the user to avoid NOTEs for a \sQuote{non visible
  binding variable} to be thrown when checking a package.
  This is possible because this function returns the loaded
  data.

  \code{ldata} loads a package data in the parent frame. It
  is a shortcut for \code{packageData(list, ...,
  envir=parent.frame())}.
}
\examples{
\dontrun{ mydata <- packageData('mydata') }
\dontrun{
# in a package' source => won't issue a NOTE
myfunction function(){
	mydata <- ldata('mydata')
}
}
}

