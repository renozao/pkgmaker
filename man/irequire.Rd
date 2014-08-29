\name{irequire}
\alias{irequire}
\title{Require a Package with User Interaction}
\usage{
  irequire(package, lib = NULL, ..., load = TRUE,
    msg = NULL, quiet = TRUE, prependLF = FALSE,
    ptype = c("CRAN-like", "BioC", "BioCsoft", "BioCann"),
    autoinstall = !interactive())
}
\arguments{
  \item{package}{name of the package}

  \item{lib}{path to the directory (library) where the package is to be
looked for and installed if agreed by the user.}

  \item{...}{extra arguments passed to \code{\link{install.packages}}.}

  \item{load}{a logical that indicates if the package should be loaded,
possibly after installation.}

  \item{msg}{message to display in case the package is not found when first
trying to load/find it.
This message is appended to the string \dQuote{Package '<packagename>' is required}.}

  \item{quiet}{logical that indicates if loading a package should be done quietly
with \code{\link{require.quiet}} or normally with \code{\link{require}}.}

  \item{prependLF}{logical that indicates if the message should start at a new line.}

  \item{ptype}{type of package: from CRAN-like repositories, Bioconductor, Bioconductor software, Bioconductor annotation.
Bioconductor packages are installed using \code{\link[repotools]{install.pkgs}} from the
\pkg{repotools} package.}

  \item{autoinstall}{logical that indicates if missing packages should just be installed
without asking with the user, which is the default in non-interactive sessions.}
}
\value{
  \code{TRUE} if the package was successfully loaded/found (installed),
\code{FALSE} otherwise.
}
\description{
  Like base \code{\link{require}}, \code{irequire} tries to
  find and load a package, but in an interactive way, i.e.
  offering the user to install it if not found.
}
\seealso{
  Other require: \code{\link{mrequire}}, \code{\link{qlibrary}}, \code{\link{qrequire}}, \code{\link{require.quiet}}
}

