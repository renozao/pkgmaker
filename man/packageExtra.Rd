\name{packageExtraHandler}
\alias{install.extrapackages}
\alias{install.extras}
\alias{packageExtra}
\alias{packageExtraHandler}
\alias{packageExtraRunner}
\title{Install/Run Extra Things After Standard Package Installation}
\usage{
  packageExtraHandler(handler, fun, package = NULL)

  packageExtra(handler, extra = NULL, ..., package = NULL,
    .wrap = FALSE)

  packageExtraRunner(handler)

  install.extras(package, extra = NULL, ...,
    .verbose = getOption("verbose"))

  install.extrapackages(package, extra = NULL, ...,
    .verbose = getOption("verbose"))
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

  \item{extra}{name of the extra action. When registering
  an action with \code{}, it should be unique within the
  package, or it will overwrite a previously registered
  action.}

  \item{...}{extra arguments passed to the actual call to
  the handler. In \code{packageExtra}, these define default
  arguments for the call, which can be overwritten by
  arguments passed to the runner function.}

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

  \code{packageExtra} registers extra actions for a given
  handler.

  \code{packageExtraRunner} defines a function to run all
  or some of the actions registered for a given handler in
  a given package. For example, the function
  \code{install.extrapackages} is the runner defined for
  the extra handler \code{'install'} via
  \code{packageExtraRunner('install')}.

  \code{install.extras} runs all extra actions registered
  for a given package.

  \code{install.extrapackages} runs all extra
  \code{'install'} actions, i.e. those registered for
  handler \code{'install'}.
}
\details{
  \code{packageExtraHandler} defines main action handler
  functions, for which default arguments are registered
  using \code{packageExtra}.

  For example, calling \code{packageExtra('install',
  pkgs='non_CRAN_pkg', repos='http://non-standard-repo')}
  in a source file of package 'myPkg' registers the call
  \code{install.packages('non_CRAN_pkg',
  repos='http://non-standard-repo', ...)} in a registry
  internal to the package. All calls to
  \code{packageExtra('install', ...)} can then be run by
  the user, as a post installation step via
  \code{install.extra('myPkg', ..)}.
}

