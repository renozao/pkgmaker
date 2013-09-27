\name{CLIArgumentParser}
\alias{CLIArgumentParser}
\alias{parseCMD}
\title{Enhanced Command Line Argument Parser}
\usage{
  CLIArgumentParser(prog = CLIfile(), description = "",
    ..., epilog = "", show.defaults = TRUE)

  parseCMD(parser, ARGS = commandArgs(TRUE), debug = FALSE,
    envir = parent.frame())
}
\arguments{
  \item{prog}{program name}

  \item{description}{program description}

  \item{...}{extra arguments passed to
  \code{\link[argparse]{ArgumentParser}}.}

  \item{epilog}{epilog messages to display at the end of
  the man pages}

  \item{show.defaults}{logical that indicates if default
  arugment values should be displayed.}

  \item{parser}{parser object as returned by
  \code{CLIArgumentParser}.}

  \item{ARGS}{command line argument to parse, as a named
  list or a character string.}

  \item{debug}{logical that indicate if debugging
  information should be printed.}

  \item{envir}{environment that contains where the
  sub-command functions are looked for.}
}
\description{
  Extends the capabilities of package \pkg{argparse}, e.g.,
  in defining sub commands.

  \code{parseCMD} parse command line arguments for
  sub-commands, and dispatch to the associated function.
}

