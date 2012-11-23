\name{packageRegistry}
\alias{packageRegistry}
\alias{setPackageRegistry}
\title{Package Registry}
\usage{
  packageRegistry(name, error = TRUE, package = NULL)

  setPackageRegistry(name, regobj, description = "", ...,
    overwrite = FALSE)
}
\arguments{
  \item{name}{Name of a sub-registry}

  \item{error}{a logical that indicate whether an error is
  thrown if the sub-registry is not found (default) or just
  \code{NULL}.}

  \item{package}{package where to look for the registry. If
  \code{NULL} this will defaults to the top caller
  package.}

  \item{regobj}{a \code{\link[registry:regobj]{registry}}
  object or a single character string that indicates the
  class of the objects that are stored in the
  sub-registry.}

  \item{description}{short description line about the
  registry. It is recommended to provide such description
  as it makes clearer the purpose of the registry. This
  description is listed}

  \item{...}{named values used to set extra information
  about the new registry, that are stored in dedicated
  fields. Currently not used, as no extra field other than
  \code{'description'} is defined.}

  \item{overwrite}{a logical that indicate if an existing
  registry with the same should be overwritten if it
  exists.}
}
\value{
  a \code{\link[registry:regobj]{registry}} object or
  \code{NULL} (see argument \code{error}).
}
\description{
  Package Registry

  Adds a registry to the package
}

