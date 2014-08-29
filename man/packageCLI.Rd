\name{packageCLI}
\alias{packageCLI}
\title{Package Specific Command Line Interface}
\usage{
  packageCLI(package, altfile = NULL, local = TRUE,
    ARGS = commandArgs(TRUE), ...)
}
\arguments{
  \item{package}{package name}

  \item{altfile}{alternative file that defines the main CLI entry point.
That is a function named \code{CLI}, which takes the list of parsed command line
arguments as its first argument.}

  \item{local}{logical that indicates if the main CLI function should be
defined and evaluated in a local environment, or in the user's Global
environment.}

  \item{ARGS}{list of parsed arguments passed to the main CLI function.}

  \item{...}{extra arugments passed to the package's CLI function.}
}
\description{
  Package Specific Command Line Interface
}

