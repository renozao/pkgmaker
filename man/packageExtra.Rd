\name{setPackageExtraHandler}
\alias{install.extrapackages}
\alias{install.extras}
\alias{packageExtra}
\alias{packageExtraHandler}
\alias{packageExtraRunner}
\alias{setPackageExtra}
\alias{setPackageExtraHandler}
\title{Install/Run Extra Things After Standard Package Installation}
\usage{
  setPackageExtraHandler(handler, fun, ...)

  packageExtraHandler(handler = NULL, ...)

  setPackageExtra(handler, extra, ...)

  packageExtra(handler = NULL, extra = NULL,
    package = NULL, .wrap = FALSE)

  packageExtraRunner(handler)

  install.extras(package, extra = NULL, handler = NULL,
    ..., .verbose = getOption("verbose"))

  install.extrapackages(package, extra = NULL,
    handler = NULL, ..., .verbose = getOption("verbose"))
}
\arguments{
  \item{handler}{name of a handler, e.g, \code{'install'}.
  It must be unique across all handlers registered by any
  other packages.}

  \item{fun}{handler function that will be called with the
  arguments registered with \code{packageExtra(name, ...)}}

  \item{package}{package name where to store/look for the
  internal registries. End users should not need to use
  this argument.}

  \item{...}{extra arguments passed to internal function
  calls. In \code{packageExtraHandler}, these are passed to
  \code{\link{pkgregfetch}}.

  In \code{setPackageExtra}, these define default arguments
  for the handler function. These are overwritten by
  arguments in the call to runner function if any.}

  \item{extra}{name of the extra action.}

  \item{.wrap}{logical that indicates if a function that
  runs the extra action should be returned or only the
  default arguments}

  \item{.verbose}{logical that indicates if verbose
  messages about the extra actions being run should be
  displayed.}
}
\value{
  the runner function associated with the newly registered
  handler, as built by \code{packageExtraRunner}.
}
\description{
  These functions define a framework to register actions
  for which default sets of arguments can be defined when
  (lazy-)loading a package, and run later on, e.g., after
  the package is installed using dedicated commands.

  \code{setPackageExtraHandler} defines main action handler
  functions, for which actions are defined as a set of
  arguments and registered using \code{setPackageExtra}.

  \code{packageExtraHandler} retrieves a given handler from
  the registry.

  For example, calling \code{setPackageExtra('install',
  pkgs='non_CRAN_pkg', repos='http://non-standard-repo')}
  in a source file of package 'myPkg' registers the call
  \code{install.packages('non_CRAN_pkg',
  repos='http://non-standard-repo', ...)} in a registry
  internal to the package. All calls to
  \code{setPackageExtra('install', ...)} can then be run by
  the user, as a post installation step via
  \code{install.extrapackages('myPkg', ..)}.

  \code{packageExtra} retrieve a given extra action, either
  as its registry entry, or as a function that would
  perform the given action.

  \code{packageExtraRunner} defines a function to run all
  or some of the actions registered for a given handler in
  a given package. For example, the function
  \code{install.extrapackages} is the runner defined for
  the extra handler \code{'install'} via
  \code{packageExtraRunner('install')}.

  \code{install.extrapackages} runs all extra actions
  registered for a given package.

  \code{install.extrapackages} is defined as the extra
  handler for the extra action handler
  \code{'install.packages'}. All arguments in \code{...}
  are passed to \code{\link{install.packages}}. By default,
  packages that are already installed are not re-installed.
  An extra argument \code{force} allows to force their
  installation. The packages are loaded if their
  installation is successful.
}

