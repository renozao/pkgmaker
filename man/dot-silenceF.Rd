\name{.silenceF}
\alias{.silenceF}
\title{Silencing Functions}
\usage{
  .silenceF(f, level = 7L)
}
\arguments{
  \item{f}{function to silence}

  \item{level}{a single numeric (integer) that indicates the silencing level, which encodes the set of
output to be silenced.

It is interpreted like unix permission bit system, where each bit of the binary expression of the silencing
level corresponds to a given type of output:
\itemize{
\item 0: nothing silenced;
\item 1: \emph{stdout};
\item 2: \emph{stderr} messages;
\item 4: \emph{stderr} warnings.
}

For example, level \code{3 = 2 + 1} means silencing \emph{stdout} and \emph{stderr}, while
\code{5 = 3 + 2} means silencing \emph{stderr} messages and warnings, but not outputs to \emph{stdout}.
The default value is \code{7 = 4 + 2 + 1}, which silences all output.

Negative values are supported and mean \emph{"silence everything except the corresponding type"},
e.g., \code{level = -1} silences all except \emph{stdout} (computed as the binary complementary of 7, i.e. \code{7 - 1 = 5 = 3 + 2}).
See examples.}
}
\value{
  a function
}
\description{
  Generates a wrapper function that silences the output,
  messages, and/or warnings of a given function.
}
\examples{
\dontshow{# roxygen generated flag
options(R_CHECK_RUNNING_EXAMPLES_=TRUE)
}

f <- function(){
	cat("stdout message\\n")
 message("stderr message")
	warning("stderr warning", immediate. = TRUE)
}

# example of generated wrapper
g <- .silenceF(f)
g

# use of silencing level
for(l in 7:-7){ message("\\nLevel: ", l); .silenceF(f, l)() }

# inline functions
ifun <- .silenceF(function(){ f(); invisible(1) })
ifun()
ifun <- .silenceF(function(){ f(); 1 })
ifun()
ifun <- .silenceF(function(){ f(); 1 }, 2L)
ifun()
}

