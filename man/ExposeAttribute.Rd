\name{ExposeAttribute}
\alias{attr_mode}
\alias{attr_mode<-}
\alias{ExposeAttribute}
\title{Exposing Object Attributes}
\usage{
  ExposeAttribute(object, ..., .MODE = "rw",
    .VALUE = FALSE)

  attr_mode(x)

  attr_mode(x)<-value
}
\arguments{
  \item{object}{any R object whose attributes need to be exposed}

  \item{...}{attributes, and optionally their respective values or
access permissions.
See argument \code{value} of \code{attr_mode} for details on the
way of specifying these.}

  \item{.MODE}{access mode:
\describe{
\item{\dQuote{r}:}{ (read-only) only method \code{$} is defined}
\item{\dQuote{w}:}{ (write-only) only method \code{$<-} is defined}
\item{\dQuote{rw}:}{ (read-write) both methods \code{$} and \code{$<-}
are defined}
}}

  \item{.VALUE}{logical that indicates if the values of named arguments
in \code{...} should be considered as attribute assignments,
i.e. that the result object has these attributes set with the specified values.
In this case all these attributes will have the access permission
as defined by argument \code{.MODE}.}

  \item{x}{an \code{ExposeAttribute} object}

  \item{value}{replacement value for mode.
It can be \code{NULL} to remove the ExposeAttribute wrapper,
a single character string to define a permission for all atributes
(e.g., \code{'rw'} or \code{'r'}), or a list specifying access permission
for specific attributes or classes of attributes defined by regular expressions.
For example, \code{list(a='r', b='w', `blabla.*`='rw')} set attribute \code{'a'}
as read-only, attribute \code{'b'} as write-only, all attributes that start with
\code{'blabla'} in read-write access.}
}
\description{
  The function \code{ExposeAttribute} creates an S3 object
  that exposes all attributes of any R object, by making
  them accessible via methods \code{\link{$}} and/or
  \code{\link{$<-}}.

  \code{attr_mode} and \code{attr_mode<-} get and sets the
  access mode of \code{ExposeAttribute} objects.
}

