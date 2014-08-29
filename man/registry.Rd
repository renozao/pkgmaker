\name{packageRegistry}
\alias{hasPackageRegistry}
\alias{packageRegistries}
\alias{packageRegistry}
\alias{setPackageRegistry}
\alias{setPackageRegistryEntry}
\title{Package Registry}
\usage{
  packageRegistry(regname = NULL, quiet = FALSE,
    entry = FALSE, update = !entry,
    package = topenv(parent.frame()))

  packageRegistries(regname = NULL, package = NULL,
    primary = FALSE)

  hasPackageRegistry(regname = NULL, package)

  setPackageRegistry(regname, regobj, description = "",
    entrydesc = NA, ..., package = topenv(parent.frame()),
    overwrite = FALSE)

  setPackageRegistryEntry(regname, key, ...,
    overwrite = FALSE, verbose = FALSE,
    where = topenv(parent.frame()), msg = NULL)
}
\arguments{
  \item{regname}{Name of a sub-registry, used as its identifier.}

  \item{quiet}{a logical that indicates that one should return the (meta-)registry if it exists,
or \code{NULL} otherwise, without throwing any error.}

  \item{entry}{logical that indicates if the corresponding meta registry entry should
be directly returned, without any other processing.}

  \item{update}{logical that indicates if the package registry should be updated, by adding/removing
entries from other loaded/unloaded packages.}

  \item{package}{package where to store or look for the registry.}

  \item{primary}{logical that indicates if only primary registries
should be listed.}

  \item{regobj}{a \code{\link[registry:regobj]{registry}} object or a single character
string that indicates the class of the objects that are stored in the
sub-registry.
See details for the list of the sub-registry's fields in this latter case.}

  \item{description}{short description line about the registry.
It is recommended to provide such description as it makes clearer the purpose of the
registry.
This description is shown when the registry object is printed/formated/listed.}

  \item{entrydesc}{human readable description that is used in log messages
when registering/removing entries.}

  \item{...}{named values used to set extra information about the new registry, that
are stored in the corresponding fields of the meta-registry.
Currently not used, as no extra field other than \code{'description'} is defined.}

  \item{overwrite}{a logical that indicate if an existing registry with the same
should be overwritten if it exists.}

  \item{key}{entry identifier.}

  \item{where}{package name or namespace that owns the registry.}

  \item{verbose}{a logical that indicates if verbosity should be toggle on.}

  \item{msg}{addon message to print at the end of the output log line,
when \code{verbose=TRUE}.}
}
\value{
  a \code{\link[registry:regobj]{registry}} object or \code{NULL} (see argument
\code{quiet}).
}
\description{
  \code{packageRegistry} provides ways to create query
  package specific registries.

  \code{packageRegistries} lists registries from loaded
  packages.

  \code{hasPackageRegistry} tells if a given package has a
  meta-registry or a given registry.

  Each package sub-registry has its own set of fields.
  Sub-registries defined by passing a character string in
  argument \code{regobj} of \code{setPackageRegistry} have
  the following fields: \code{'key'} and \code{'object'}

  \code{setPackageRegistryEntry} adds an entry in a package
  registry.
}
\details{
  Package registries are organised in a meta-registry (a
  registry of registries) within a package's namespace.
  Each registry can be used to store sets of built-in or
  user-defined objects in an organised way, e.g. algorithms
  or datasets.

  A package meta-registry is a
  \code{\link[registry:regobj]{registry}} object, whose
  entries are \code{\link[registry:regobj]{registry}}
  objects themselves. A sub-registry entry is defined by
  the following fields: \describe{ \item{key}{The
  sub-registry's accession key/identifier (a character
  string).} \item{regobj}{The sub-registry itself (a
  \code{registry} object)} \item{description}{Human
  readable description of the purpose of the registry (a
  character string)} \item{description}{Short human
  readable description of the type of entries (a character
  string)} \item{package}{owner package, which is forced to
  be the package in which the meta registry is defined.}
  \item{parent}{The name of the package that holds the
  parent registry, which we call the primary package. This
  field is non empty for cross-package registries, i.e.
  registries that derive from primary package's own
  registry. Their entries are defined when (lazy-)loading
  the dependent package's namespace.} }

  Note that this function cannot be called from the global
  environment, but from a package namespace, e.g., when a
  package is lazy-loaded on installation or loaded via the
  function \code{\link[devtools]{load_all}} from the
  \pkg{devtools} package.
}

