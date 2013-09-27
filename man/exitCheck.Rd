\name{exitCheck}
\alias{exitCheck}
\title{Exit Error Checks}
\usage{
  exitCheck()
}
\description{
  \code{exitCheck} provides a mechanism to distinguish the
  exit status in \code{\link{on.exit}} expressions.
}
\details{
  It generates a function that is used wihtin a function's
  body to "flag" normal exits and in its
  \code{\link{on.exit}} expression to check the exit status
  of a function. Note that it will correctly detect errors
  only if all normal exit are wrapped into a call to it.
}
\examples{
# define some function
f <- function(err){

 # initialise an error checker
	success <- exitCheck()

 # do something on exit that depends on the error status
	on.exit({
		if(success()) cat("Exit with no error: do nothing\\n")
		else cat("Exit with error: cleaning up the mess ...\\n")
	})

 # throw an error here
	if( err ) stop('There is an error')

	success(1+1)
}

# without error
f(FALSE)
# with error
try( f(TRUE) )
}

