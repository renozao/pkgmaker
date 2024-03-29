% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/devutils.R
\name{packageEnv}
\alias{packageEnv}
\alias{topns_name}
\alias{topns}
\alias{packageName}
\alias{str_ns}
\alias{packagePath}
\alias{isPackageInstalled}
\alias{as_package}
\title{Package Development Utilities}
\usage{
packageEnv(pkg, skip = FALSE, verbose = FALSE)

topns_name(n = 1L, strict = TRUE, unique = TRUE)

topns(strict = TRUE)

packageName(envir = packageEnv(), .Global = FALSE, rm.prefix = TRUE)

str_ns(envir = packageEnv())

packagePath(..., package = NULL, lib.loc = NULL, check = TRUE)

isPackageInstalled(..., lib.loc = NULL)

as_package(x, ..., quiet = FALSE, extract = FALSE)
}
\arguments{
\item{pkg}{package name. If missing the environment of the runtime caller
package is returned.}

\item{skip}{a logical that indicates if the calling namespace should be
skipped.}

\item{verbose}{logical that toggles verbosity}

\item{n}{number of namespaces to return}

\item{strict}{a logicical that indicates if the global environment should
be considered as a valid namespace.}

\item{unique}{logical that indicates if the result should be reduced
to contain only one occurence of each namespace.}

\item{envir}{environment where to start looking for a package name.
The default is to use the \strong{runtime} calling package environment.}

\item{.Global}{a logical that indicates if calls from the global
environment should throw an error (\code{FALSE}: default) or the string
\code{'R_GlobalEnv'}.}

\item{rm.prefix}{logical that indicates if an eventual prefix 'package:'
should be removed from the returned string.}

\item{...}{arguments passed to \code{\link{file.path}}.}

\item{package}{optional name of an installed package}

\item{lib.loc}{path to a library of R packages where to search the package}

\item{check}{logical that indicates if an error should be thrown if the path to the
package root directory cannot be found.
If this is the case and \code{check = FALSE}, then the function returns \code{NULL}.}

\item{x}{package specified by its installation/development path or its name
as \code{'package:*'}.}

\item{quiet}{a logical that indicate if an error should be thrown if a
package is not found. It is also passed to \code{\link{find.package}}.}

\item{extract}{logical that indicates if DESCRIPTION of package
source files should be extracted.
In this case there will be no valid path.}
}
\value{
\itemize{
\item \code{packageEnv} returns an environment
}

\itemize{
\item \code{topns_name} returns the name of the namespace a character string.
}

\itemize{
\item \code{topns} returns an environment.
}

\itemize{
\item \code{packageName} returns a character string
}

\itemize{
\item \code{packagePath} returns a character string.
}

\itemize{
\item \code{as_package} returns a \code{package} object like \link[devtools:as.package]{devtools::as.package}.
}
}
\description{
\code{packageEnv} is a slight modification from \code{\link{topenv}}, which
returns the top environment, which in the case of development
packages is the environment into which the source files are loaded by
\code{\link[devtools]{load_all}}.
}
\section{Functions}{
\itemize{
\item \code{topns_name()}: returns the name of the runtime sequence of top namespace(s),
i.e. the name of the top calling package(s), from top to bottom.

The top namespace is is not necessarily the namespace where \code{topns_name}
is effectively called.
This is useful for packages that define functions that need to access the
calling namespace, even from calls nested into calls to another function from
the same package -- in which case \code{topenv} would not give the desired
environment.

\item \code{topns()}: returns the runtime top namespace, i.e. the namespace of
the top calling package, possibly skipping the namespace where \code{topns}
is effectively called.
This is useful for packages that define functions that need to access the
calling namespace, even from calls nested into calls to another function from
the same package -- in which case \code{topenv} would not give the desired
environment.

\item \code{packageName()}: returns the current package's name.
It was made internal from version 0.16, since the package \pkg{utils}
exported its own \code{\link[utils]{packageName}} function in R-3.0.0.

\item \code{str_ns()}: formats a package environment/namespace for log/info messages.

\item \code{packagePath()}: returns the current package's root directory, which is
its installation/loading directory in the case of an installed package, or
its source directory served by devtools.

\item \code{isPackageInstalled()}: checks if a package is installed.
\itemize{
\item \code{isPackageInstalled} returns a logical flag.
}

\item \code{as_package()}: an enhanced version of \code{\link[devtools]{as.package}},
that is not exported not to mask the original function.
It could eventually be incorporated into \code{devtools} itself.
Extra arguments in \code{...} are passed to \code{\link{find.package}}.

}}
