\name{postponeAction}
\alias{postponeAction}
\alias{runPostponedAction}
\title{Postponing Actions}
\usage{
  postponeAction(expr, key = digest(tempfile()),
    group = NULL, envir = topns(strict = FALSE),
    verbose = getOption("verbose"))

  runPostponedAction(group = NULL,
    verbose = getOption("verbose"))
}
\arguments{
  \item{expr}{expression that define the action to
  postpone. Currently only functions are supported.}

  \item{key}{identifier for this specific action. It should
  be unique across the postponed actions from the same
  group.}

  \item{group}{optional parent action group. This enables
  to define meaningful sets of actions that can be run all
  at once.}

  \item{envir}{environment in which the action should be
  executed. Currently not used.}

  \item{verbose}{logical that toggles verbose messages.}
}
\description{
  This function implement a mechanism to postpone actions,
  which can be executed at a later stage. This is useful
  when developing packages, where actions that need to be
  run in the \code{link{.onLoad}} function but can be
  defined close to their context.
}
\examples{
\dontshow{# roxygen generated flag
options(R_CHECK_RUNNING_EXAMPLES_=TRUE)
}

opt <- options(verbose=2)

# define actions
postponeAction(function(){print(10)}, "print")
postponeAction(function(){print(1:10)}, "more")
postponeAction()
# execute actions
runPostponedAction()
runPostponedAction()

# restore options
options(opt)
}

