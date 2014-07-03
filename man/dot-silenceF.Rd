\name{.silenceF}
\alias{.silenceF}
\title{Silencing Functions}
\usage{
  .silenceF(f, level = 7L)
}
\arguments{
  \item{f}{function to silence}

  \item{level}{silencing level, which specifies the type of
  output to silence. It is encoded using the following
  bits: 0 = nothing silenced, 1 = stdout only, 2 = stderr
  only, 4 = warnings only. Negative values means "silence
  everything except the corresponding type", e.g., -1L =
  silence all except stdout. See examples.}
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
	cat("stdout\\n")
 message("stderr")
	warning("warning", immediate. = TRUE)
}

# example of generated wrapper
g <- .silenceF(f)
g

# use of silencing level
for(l in 7:-7){ message("\\nLevel: ", l); .silenceF(f, l)() }

# inline functions
f <- .silenceF(function() invisible(1))
f()
f <- .silenceF(function() 1)
f()
}

