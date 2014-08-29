\name{setupPackageOptions}
\alias{setupPackageOptions}
\title{Package Specific Options}
\usage{
  setupPackageOptions(..., NAME = NULL,
    ENVIR = topenv(parent.frame()),
    RESET = isLoadingNamespace())
}
\arguments{
  \item{...}{a single named list or named arguments that provide the default
options and their values.}

  \item{NAME}{name of the set of options.
This is used as a prefix for the name of the associated global
option: \code{package:<name>}.}

  \item{ENVIR}{environment where the option wrapper functions will be defined.
No function is defined if \code{ENVIR=NULL}}

  \item{RESET}{a logical that indicates whether the option set should overwrite
one that already exists if necessary.
The default is \code{FALSE} (i.e. no reset), except when loading a namespace,
either from an installed package or a development package -- with devtools.
If \code{FALSE}, an error is thrown if trying to setup options with the same name.}
}
\description{
  The following functions to access/set the options from
  the set are assigned in \code{envir}: \describe{
  \item{<subset>Options}{} \item{<subset>GetOption}{} }
}

