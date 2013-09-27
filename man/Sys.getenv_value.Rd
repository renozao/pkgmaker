\name{Sys.getenv_value}
\alias{Sys.getenv_value}
\title{System Environment Variables}
\usage{
  Sys.getenv_value(name, raw = FALSE)
}
\arguments{
  \item{name}{variable name as a character string.}

  \item{raw}{logical that indicates if one should return
  the raw value or the convertion of any false value to
  \code{FALSE}.}
}
\value{
  the value of the environment variable as a character
  string or \code{NA} is the variable is not defined
  \strong{at all}.
}
\description{
  System Environment Variables
}
\examples{
# undefined returns FALSE
Sys.getenv_value('TOTO')
# raw undefined returns NA
Sys.getenv_value('TOTO', raw = TRUE)

Sys.setenv(TOTO='bla')
Sys.getenv_value('TOTO')

# anything false-like returns FALSE
Sys.setenv(TOTO='false'); Sys.getenv_value('TOTO')
Sys.setenv(TOTO='0'); Sys.getenv_value('TOTO')

# cleanup
Sys.unsetenv('TOTO')
}

