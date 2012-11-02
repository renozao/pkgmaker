\name{simpleRegistry}
\alias{simpleRegistry}
\title{Simple Package Registry}
\usage{
  simpleRegistry(name, envir = topenv(parent.frame()),
    verbose = FALSE)
}
\arguments{
  \item{name}{name of the registry object, with which it
  will be assigned in \code{envir}.}

  \item{envir}{environment where to store the registry
  object. Defaults to the caller's top environment.}

  \item{verbose}{logical that toggle a verbose message when
  the object is first created.}
}
\description{
  Simple Package Registry
}

