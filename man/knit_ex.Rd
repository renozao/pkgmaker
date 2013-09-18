\name{knit_ex}
\alias{hook_try}
\alias{knit_ex}
\title{Knitr Extensions}
\usage{
  knit_ex(x, ..., quiet = TRUE)

  hook_try(before, options, envir)
}
\arguments{
  \item{x}{text to knit as a character vector}

  \item{...}{arguments passed to
  \code{\link[knitr]{knit2html}} or
  \code{\link[knitr]{knit}}}

  \item{quiet}{logical that indicates if knitting should be
  quiet (no progress bars etc..).}

  \item{before}{logical that indicates when the hook is
  being called: before or after the chunk is processed.}

  \item{options}{list of current knitr chunk options}

  \item{envir}{environment where the chunk is evaluated}
}
\description{
  \code{knit_ex} is a utility function for running small
  knitr examples, e.g., to illustrate functionnalities or
  issues.

  \code{hook_try} simply defines a function \code{try} in
  \code{envir} that prints the error message if any, and is
  called instead of base \code{\link{try}}.
}
\examples{
library(knitr)
knit_ex("1 + 1")
library(knitr)

# standard error message is caught
knit_ex("stop('ah ah')")

# with try the error is output on stderr but not caughted by knitr
knit_ex("try( stop('ah ah') )")

# no message caught
knit_ex("
```{r, include = FALSE}
knit_hooks$set(try = pkgmaker::hook_try)
```

```{r, try=TRUE}
try( stop('ah ah') )
```")
}

