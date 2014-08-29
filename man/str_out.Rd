\name{str_out}
\alias{str_bs}
\alias{str_desc}
\alias{str_fun}
\alias{str_out}
\title{Formatting Utilities}
\usage{
  str_out(x, max = 3L, quote = is.character(x),
    use.names = FALSE, sep = ", ", total = FALSE)

  str_desc(object, exdent = 0L)

  str_fun(object)

  str_bs(x)
}
\arguments{
  \item{x}{character vector}

  \item{max}{maximum number of values to appear in the list. If \code{x} has
more elements than \code{max}, a \code{"..."} suffix is appended.}

  \item{quote}{a logical indicating whether the values should be quoted with
single quotes (defaults) or not.}

  \item{use.names}{a logical indicating whether names should be added to the
list as \code{NAME=VAL, ...} or not (default).}

  \item{sep}{separator character}

  \item{total}{logical that indicates if the total number of elements should be
appended to the formatted string as \code{"'a', ..., 'z' (<N> total)"}.}

  \item{object}{an R object}

  \item{exdent}{extra indentation passed to str_wrap, and used if the output
should spread over more than one lines.}
}
\value{
  a single character string
}
\description{
  \code{str_out} formats character vectors for use in show
  methods or error/warning messages.

  \code{str_desc} builds formatted string from a list of
  complex values.

  \code{str_fun} extracts and formats a function signature.
  It typically formats the output
  \code{capture.output(args(object))}.

  \code{str_bs} substitutes backspace characters (\\b) to
  produce a character string as it would be displayed in
  the console.
}
\examples{
\dontshow{# roxygen generated flag
options(R_CHECK_RUNNING_EXAMPLES_=TRUE)
}

#----------
# str_out
#----------
x <- letters[1:10]
str_out(x)
str_out(x, 8)
str_out(x, Inf)
str_out(x, quote=FALSE)
str_out(x, total = TRUE)

#----------
# str_fun
#----------
str_fun(install.packages)

#----------
# str_bs
#----------
# Backspace substitution
str_bs("abc")
str_bs("abc\\b")
str_bs("abc\\b\\b")
str_bs("abc\\bd")
str_bs("abc\\b\\bde\\b")

# more complex example
x <- "\\bab\\nc\\bd\\n\\babc\\b\\bd"
cat(x, "\\n")
y <- str_bs(x)
y
cat(y, "\\n")
}
\author{
  Renaud Gaujoux

\code{str_bs} was adapted from a proposal from Yihui Xie.
}

