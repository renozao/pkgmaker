\name{userData}
\alias{userData}
\title{User Data Directory}
\usage{
  userData(..., create = NULL,
    package = topenv(parent.frame()))
}
\arguments{
  \item{...}{path parts passed to \code{\link{file.path}} to be appended to
the main path.}

  \item{create}{logical that indicates if the \strong{base} directory should be
created if it does not exists.

Note that directories -- and files -- under the base directory are not automatically
created. The user should therefore care of it in the caller function if necessary.

If \code{create=TRUE}, then the base directory is forced to be created in the user's home directory.
If \code{create=FALSE}, then the base directory is never created.

See also section \emph{Details}.}

  \item{package}{name of the package associated with the user data path.
It is used to prefix the path, within the user R data directory.}
}
\description{
  \code{userData} returns the path to a local
  directory/file where package-related user data can be
  stored. Note that a base directory is \strong{always}
  created if necessary (see details).
}
\details{
  The package-specific user data base directory is the
  sub-directory \emph{R-data/}, located in the user's home.

  If in interactive mode, and the base directory does not
  exist yet, the user is asked if it should be created in
  his home directory. Otherwise, or if the user does not
  allow the creation in his home, this directory is created
  in the current R session's temporary directory.
}
\seealso{
  \code{\link{tempdir}}
}

