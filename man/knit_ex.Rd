\name{knit_ex}
\alias{hook_backspace}
\alias{hook_toggle}
\alias{hook_try}
\alias{knit_ex}
\title{Knitr Extensions}
\usage{
  knit_ex(x, ..., quiet = TRUE, open = FALSE)

  hook_try(before, options, envir)

  hook_backspace()

  hook_toggle()
}
\arguments{
  \item{x}{text to knit as a character vector}

  \item{...}{arguments passed to \code{\link[knitr]{knit2html}} or \code{\link[knitr]{knit}}}

  \item{quiet}{logical that indicates if knitting should be quiet (no progress bars etc..).}

  \item{open}{logical, only used when \code{x} is in .Rmd format, that indicates
if the generated document result should be open in a browse, instead of
being printed on screen.
Not that a browser will not open in non-interactive sessions, and the result will
be returned invisibly.}

  \item{before}{logical that indicates when the hook is being called:
before or after the chunk is processed.}

  \item{options}{list of current knitr chunk options}

  \item{envir}{environment where the chunk is evaluated}
}
\value{
  \code{knit_ex} returns the generated code, although invisibly when \code{open=TRUE}.
}
\description{
  \code{knit_ex} is a utility function for running small
  knitr examples, e.g., to illustrate functionalities or
  issues.

  \code{hook_try} simply defines a function \code{try} in
  \code{envir} that prints the error message if any, and is
  called instead of base \code{\link{try}}.

  \code{hook_backspace} is a chunk hook that enables the
  use of backspace characters in the output (e.g., as used
  in progress bars), and still obtain a final output as in
  the console.

  \code{hook_toggle} is a chunk hook that adds clickable
  elements to toggle \emph{indvidual} code chunks in HTML
  documents generated from .Rmd files.
}
\examples{
\dontshow{# roxygen generated flag
options(R_CHECK_RUNNING_EXAMPLES_=TRUE)
}

#----------
# knit_ex
#----------
library(knitr)
knit_ex("1 + 1")

#----------
# hook_try
#----------
library(knitr)

# standard error message is caught
knit_ex("stop('ah ah')")

# with try the error is output on stderr but not caughted by knitr
knit_ex("try( stop('ah ah') )")

# no message caught
knit_ex("
^^^{r, include = FALSE}
knit_hooks$set(try = pkgmaker::hook_try)
^^^

^^^{r, try=TRUE}
try( stop('ah ah') )
^^^")

#----------
# hook_backspace
#----------
# Correctly formatting backspaces in chunk outputs
tmp <- tempfile(fileext = '.Rmd')
cat(file = tmp, "
^^^{r, include = FALSE}
library(knitr)
knit_hooks$set(backspace = pkgmaker::hook_backspace())
^^^
Default knitr does not handle backspace and adds a special character:
^^^{r}
cat('abc\\bd')
^^^

Using the hook backspace solves the issue:
^^^{r, backspace=TRUE}
cat('abc\\bd')
^^^
")

# knit
out <- knitr::knit2html(tmp, fragment.only = TRUE)
# look at output
\dontrun{
  browseURL(out)
  edit( file = out)
}
# cleanup
unlink(c(tmp, out))

#----------
# hook_toggle
#----------
knit_ex("

Declare chunk hook:
^^^{r, setup}
library(knitr)
knit_hooks$set(toggle = hook_toggle())
^^^

The R code of this chunk can be toggled on/off, and starts visible:
^^^{r, toggle=TRUE}
print(1:10)
^^^
The R code of this chunk can be toggled on/off, and starts hidden:
^^^{r, toggle=FALSE}
print(1:10)
^^^

This is a plain chunk that cannot be toggled on/off:
^^^{r}
print(1:10)
^^^

Now all chunks can be toggled and start visible:
^^^{r, toggle_all}
opts_chunk$set(toggle = TRUE)
^^^

^^^{r}
sample(5)
^^^

To diable the toggle link, one can pass anything except TRUE/FALSE:
^^^{r, toggle = NA}
sample(5)
^^^

", open = TRUE)
}

