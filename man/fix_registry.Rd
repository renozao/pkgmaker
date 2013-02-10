\name{fix_registry}
\alias{fix_registry}
\title{Fix Registry Access Functions}
\usage{
  fix_registry(regobj)
}
\arguments{
  \item{regobj}{registry object}
}
\description{
  Fixes some of the member functions for registry objects,
  as defined in the \pkg{registry} package. The main fixed
  issue is due to the fact that key filtering does not
  return the correct entry, when an entry key is a prefix
  of another entry key, even when passed the exact key.
}
\keyword{internal}

