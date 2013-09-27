\name{add_lib}
\alias{add_lib}
\title{Adding Package Libraries}
\usage{
  add_lib(..., append = FALSE)
}
\arguments{
  \item{...}{paths to add to .libPath}

  \item{append}{logical that indicates that the paths
  should be appended rather than prepended.}
}
\description{
  Prepend/append paths to the library path list, using
  \code{\link{.libPaths}}.
}
\details{
  This function is meant to be more convenient than
  \code{.libPaths}, which requires more writing if one
  wants to: \itemize{ \item sequentially add libraries;
  \item append and not prepend new path(s); \item keep the
  standard user library in the search path. }
}
\examples{
ol <- .libPaths()
# called sequentially, .libPaths only add the last library
show( .libPaths('.') )
show( .libPaths(tempdir()) )
# restore
.libPaths(ol)

# .libPaths does not keep the standard user library
show( .libPaths() )
show( .libPaths('.') )
# restore
.libPaths(ol)

# with add_lib
show( add_lib('.') )
show( add_lib(tempdir()) )
show( add_lib('..', append=TRUE) )

# restore
.libPaths(ol)
}

