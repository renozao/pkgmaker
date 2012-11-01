\name{checkPlot}
\alias{checkPlot}
\title{Plot in Unit Tests}
\usage{
  checkPlot(expr, msg = NULL, width = 1000, height = NULL)
}
\arguments{
  \item{expr}{expression that generate th eplot}

  \item{width}{plot width}

  \item{height}{plot height (not used)}

  \item{msg}{plot msg explaining the plot . It will be used
  as the caption}
}
\description{
  Saves a plot in a PNG file that will be included in unit
  test HTML reports.
}
\keyword{internal}

