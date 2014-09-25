\name{askUser}
\alias{askUser}
\title{User Queries}
\usage{
  askUser(msg, allowed = c("y", "n"), idefault = "n",
    default = "n", case.sensitive = FALSE)
}
\arguments{
  \item{idefault}{default response in interactive mode.
This answer will be in upper case in the question and will be the one returned if the
user simply hits return.}

  \item{default}{default response in non-interactive mode.

If \code{NA}, then the user is forced to provide an answer, even in non-interactive mode
(e.g., when run through \code{Rscript}).}

  \item{msg}{The output message}

  \item{allowed}{Allowed input from the user}

  \item{case.sensitive}{Is the response case sensitive? Defaults to
    \code{FALSE}}
}
\value{
  the character string typed/agreed by the user or directly the default answer in
non-interactive mode.
}
\description{
  This function is an improved version of
  \code{\link[Biobase]{userQuery}} and ask the user about
  some task that needs her intervention to proceed, e.g.,
  ask if one should perform a computation, install a
  package, etc..
}

