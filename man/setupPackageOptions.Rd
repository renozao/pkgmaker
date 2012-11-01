\name{setupPackageOptions}
\alias{setupPackageOptions}
\title{Package Specific Options}
\usage{
  setupPackageOptions(..., NAME = NULL,
    ENVIR = parent.frame(), RESET = FALSE)
}
\arguments{
  \item{...}{a single named list or named arguments that
  provide the default options and their values.}

  \item{NAME}{name of the set of options. This is used as a
  prefix for the name of the associated global option:
  \code{package:<name>}.}

  \item{ENVIR}{environment where the option wrapper
  functions will be defined. No function is defined if
  \code{ENVIR=NULL}}

  \item{RESET}{a logical that indicates whether the option
  set should overwrite one that already exists if
  necessary. The default is \code{FALSE} (i.e. no reset),
  because one generally wants to keep options possibly
  saved in a reloaded workspace.}
}
\description{
  The following functions to access/set the options from
  the set are assigned in \code{envir}: \describe{
  \item{<subset>Options}{} \item{<subset>GetOption}{} }
}

